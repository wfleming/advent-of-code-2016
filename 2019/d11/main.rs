extern crate num_bigint;
extern crate num_traits;

use num_traits::cast::FromPrimitive;
use num_traits::cast::ToPrimitive;
use num_traits::{Zero, One};
use num_bigint::BigInt;
use std::vec::Vec;
use std::collections::HashMap;

// standard euclidian 2d space for once: y goes up, x goes right
type Coord = (i32, i32);

enum Color {
    Black,
    White,
}

impl Color {
    fn from_bigint(x: &BigInt) -> Self {
        if x == &Zero::zero() {
            Self::Black
        } else if x == &One::one() {
            Self::White
        } else {
            panic!("invalid color")
        }
    }

    fn as_bigint(&self) -> BigInt {
        match self {
            Self::Black => Zero::zero(),
            Self::White => One::one(),
        }
    }
}

enum Dir {
    Up,
    Right,
    Down,
    Left,
}

impl Dir {
    fn step(&self, pos: Coord) -> Coord {
        match self {
            Self::Up => (pos.0, pos.1 + 1),
            Self::Right => (pos.0 + 1, pos.1),
            Self::Down => (pos.0, pos.1 - 1),
            Self::Left => (pos.0 - 1, pos.1),
        }
    }

    fn turn_left(&self) -> Dir {
        match self {
            Self::Up => Self::Left,
            Self::Right => Self::Up,
            Self::Down => Self::Right,
            Self::Left => Self::Down,
        }
    }

    fn turn_right(&self) -> Dir {
        match self {
            Self::Up => Self::Right,
            Self::Right => Self::Down,
            Self::Down => Self::Left,
            Self::Left => Self::Up,
        }
    }
}

#[derive(Debug)]
#[derive(Clone)]
struct IntcodeMachine {
    tape: HashMap<BigInt, BigInt>,
    pos: BigInt,
    inputs: Vec<BigInt>,
    outputs: Vec<BigInt>,
    relative_base: BigInt,
}

impl IntcodeMachine {
    fn new(tape: Vec<BigInt>) -> IntcodeMachine {
        // convert the vec into a hashmap to support sparse tapes using bigints
        let mut tape_map: HashMap<BigInt, BigInt> = HashMap::new();

        for (pos, v) in tape.iter().enumerate() {
            tape_map.insert(
                BigInt::from_usize(pos).expect("should be parseable"),
                v.clone(),
            );
        }

        IntcodeMachine {
            tape: tape_map,
            pos: Zero::zero(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            relative_base: Zero::zero(),
        }
    }

    fn push_input(&mut self, v: BigInt) {
        self.inputs.push(v);
    }

    fn run_until_exit(&mut self) {
        while !self.is_exited() {
            self.step();
        }
    }

    fn run_until_exit_or_input_wait(&mut self) {
        loop {
            if self.is_exited() {
                break;
            }
            if self.cur_opcode() == 3 && self.inputs.len() == 0 {
                break;
            }
            self.step();
        }
    }

    fn outputs_to_s(&self) -> Vec<String> {
        self.outputs.iter().map(|x| x.to_str_radix(10)).collect()
    }


    fn step(&mut self) {
        // println!(
        //     "DEBUG: step pos={} rel_base={} opcode={} from tape_at_pos={} tape_at_100={} tape_at_101={}",
        //     self.pos, self.relative_base, self.cur_opcode(), self.tape_at(self.pos.clone()), self.tape_at(BigInt::from_i32(100).unwrap()), self.tape_at(BigInt::from_i32(101).unwrap())
        // );
        match self.cur_opcode() {
            1 => self.add(),
            2 => self.mult(),
            3 => self.read_input(),
            4 => self.write_output(),
            5 => self.jmp_if_true(),
            6 => self.jmp_if_false(),
            7 => self.less_than(),
            8 => self.equals(),
            9 => self.mod_relative_base(),
            99 => (), // halt
            _ => panic!("bad machine state, invalid opcode from '{:?}'", self.tape.entry(self.pos.clone())),
        };
    }

    fn add(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.nth_literal_param(3);
        // println!("DEBUG: add {}, {} -> [{}]", l, r, dest_pos);
        self.tape.insert(dest_pos, l + r);
        self.pos += 4;
    }

    fn mult(&mut self) {
        let l = self.nth_param(1);
        let r = self.nth_param(2);
        let dest_pos = self.nth_literal_param(3);
        // println!("DEBUG: mult {}, {} -> [{}]", l, r, dest_pos);
        self.tape.insert(dest_pos, l * r);
        self.pos += 4;
    }

    fn read_input(&mut self) {
        // println!("DEBUG: machine reading inputs. input queue={:?}", self.inputs);
        let dest_pos = self.nth_literal_param(1);
        self.tape.insert(dest_pos, self.inputs.remove(0));
        // println!("DEBUG: machine reading inputs. got dest_pos={}, input={}, full tape={:?}", dest_pos, self.tape[dest_pos], self.tape);
        self.pos += 2;
    }

    fn write_output(&mut self) {
        let o = self.nth_param(1);
        // println!("DEBUG: output {}", o);
        self.outputs.push(o);
        self.pos += 2;
    }

    fn jmp_if_true(&mut self) {
        if self.nth_param(1) != Zero::zero() {
            self.pos = self.nth_param(2);
        } else {
            self.pos += 3;
        }
    }

    fn jmp_if_false(&mut self) {
        if self.nth_param(1) == Zero::zero() {
            self.pos = self.nth_param(2);
        } else {
            self.pos += 3;
        }
    }

    fn less_than(&mut self) {
        let dest_pos = self.nth_literal_param(3);
        if self.nth_param(1) < self.nth_param(2) {
            self.tape.insert(dest_pos, One::one());
        } else {
            self.tape.insert(dest_pos, Zero::zero());
        }
        self.pos += 4;
    }

    fn equals(&mut self) {
        let dest_pos = self.nth_literal_param(3);
        if self.nth_param(1) == self.nth_param(2) {
            self.tape.insert(dest_pos, One::one());
        } else {
            self.tape.insert(dest_pos, Zero::zero());
        }
        self.pos += 4;
    }

    fn mod_relative_base(&mut self) {
        self.relative_base += self.nth_param(1);
        self.pos += 2;
    }

    fn nth_literal_param(&self, n: u32) -> BigInt {
        let val = self.tape_at(self.pos.clone() + n);
        match self.nth_param_mode(n) {
            0 => val,
            1 => panic!("invalid use of mode 1"),
            2 => self.relative_base.clone() + val,
            _ => panic!("invalid mode"),
        }
    }

    fn nth_param(&self, n: u32) -> BigInt {
        let val = self.tape_at(self.pos.clone() + n);

        match self.nth_param_mode(n) {
            0 => self.tape_at(val),
            1 => val,
            2 => self.tape_at(self.relative_base.clone() + val),
            _ => panic!("invalid mode"),
        }
    }

    fn is_exited(&self) -> bool {
          99 == self.cur_opcode()
    }

    fn tape_at(&self, pos: BigInt) -> BigInt {
        self.tape.get(&pos).unwrap_or(&Zero::zero()).clone()
    }

    fn nth_param_mode(&self, n: u32) -> u32 {
        assert!(n >= 1 && n <=3);
        let div = BigInt::from_i32((10 as i32).pow(n + 1)).expect("div a bigint");
        let ten = BigInt::from_i32(10).expect("parse 10");
        let v_big = (self.tape_at(self.pos.clone()) / div) % ten;
        v_big.to_u32().expect("should be small enough")
    }

    fn cur_opcode(&self) -> i32 {
        // each digit in the tape entry is (comma sep):
        // p3mode,p2mode,p1mode,0,opcode
        let one_hundred = BigInt::from_i32(100).expect("100");
        let tape_v: BigInt = self.tape_at(self.pos.clone());
        let op_big: BigInt = tape_v % one_hundred;
        op_big.to_i32().expect("should be small enough")
    }
}

fn read_tape(line: &str) -> Vec<BigInt> {
    line.split(",").map(|x| {
        BigInt::parse_bytes(x.trim().as_bytes(), 10)
            .expect(&format!("'{}' could not be parsed", x))
    }).collect()
}

fn input_tape() -> Vec<BigInt> {
    read_tape(include_str!("input.txt"))
}

fn paint() -> HashMap<Coord, Color> {
    let mut bot_pos: Coord = (0, 0);
    let mut panels = HashMap::new();
    let mut machine = IntcodeMachine::new(input_tape());
    let mut dir = Dir::Up;
    let mut output_pos = 0;

    while !machine.is_exited() {
        machine.run_until_exit_or_input_wait();

        // push input indicating current color
        let current_color = panels.get(&bot_pos).unwrap_or(&Color::Black);
        machine.push_input(current_color.as_bigint());

        // read outputs, if any
        if machine.outputs.len() > output_pos {
            assert_eq!(machine.outputs.len(), output_pos + 2, "always 2 there are");

            let paint_color = Color::from_bigint(&machine.outputs[output_pos]);
            panels.insert(bot_pos, paint_color);

            let turn_dir = machine.outputs[output_pos + 1].clone();
            if turn_dir == Zero::zero() {
                dir = dir.turn_left();
            } else if turn_dir == One::one() {
                dir = dir.turn_right();
            } else {
                panic!("invalid turn value");
            }

            bot_pos = dir.step(bot_pos);

            output_pos += 2;
        }
    }

    panels
}

fn panels_as_str(panels: &HashMap<Coord, Color>) -> String {
    let mut out_str = String::new();
    let x_min = panels.keys().map(|p| p.0).min().unwrap();
    let x_max = panels.keys().map(|p| p.0).max().unwrap();
    let y_min = panels.keys().map(|p| p.1).min().unwrap();
    let y_max = panels.keys().map(|p| p.1).max().unwrap();

    for y in y_min..(y_max + 1) {
        for x in x_min..(x_max + 1) {
            let c = panels.get(&(x, y)).unwrap_or(&Color::Black);

            match c {
                Color::White => out_str.push_str("#"),
                _ => out_str.push_str(" "),
            }
        }
        out_str.push_str("\n");
    }

    out_str
}

fn main() {
    // p1
    let panels = paint();
    println!("p1: painted panels count = {}", panels.len());

    // p2
    println!("p2:");
    println!("{}", panels_as_str(&panels));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bigint_math() {
        let x = BigInt::from_i32(100).unwrap();
        let y = x + 1;
        assert_eq!(BigInt::from_i32(101).unwrap(), y);
    }

    #[test]
    fn test_p1_quine() {
        let tape = read_tape("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");

        let mut machine = IntcodeMachine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(tape, machine.outputs);
    }

    #[test]
    fn test_p1_a() {
        let tape = read_tape("1102,34915192,34915192,7,4,7,99,0");

        let mut machine = IntcodeMachine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(16, machine.outputs[0].to_str_radix(10).len());
    }

    #[test]
    fn test_p1_b() {
        let tape = read_tape("104,1125899906842624,99");

        let mut machine = IntcodeMachine::new(tape.clone());
        machine.run_until_exit();
        assert_eq!(tape[1], machine.outputs[0]);
    }
}
