FROM ruby:2.5-slim

RUN apt-get -y update
RUN apt-get -y install \
  build-essential \
  imagemagick \
  tesseract-ocr

RUN gem install mini_magick rtesseract chunky_png
