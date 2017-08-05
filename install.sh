#!/bin/bash
sudo dnf install \
  autoconf \
  chrpath \
  curl \
  libunwind \
  freetype-devel \
  gmp-devel \
  java-1.8.0-openjdk-devel \
  jpackage-utils \
  libarchive-devel \
  libICE-devel \
  libjpeg-turbo-devel \
  libSM-devel \
  libX11-devel \
  libXaw-devel \
  libXext-devel \
  libXft-devel \
  libXinerama-devel \
  libXmu-devel \
  libXpm-devel \
  libXrender-devel \
  libXt-devel \
  ncurses-devel \
  openssl-devel \
  pkgconfig \
  readline-devel \
  libedit-devel \
  unixODBC-devel \
  zlib-devel \
  uuid-devel ;
git clone https://github.com/SWI-Prolog/swipl-devel && \
  cd swipl-devel && \
  ./prepare --yes && \
  cp build.templ build && \
  ./build;

findexe()
{ oldifs="$IFS"
  IFS=:
  for d in $PATH; do
    if [ -x $d/$1 ]; then
       IFS="$oldifs"
       return 0
    fi
  done
  IFS="$oldifs"
  return 1
}

if [ -z "$SWIPL" ]; then
  findexe swipl;
  SWIPL=swipl
fi
if [ -z "$SWIPL" ]; then
  echo "ERROR: Cannot find SWI-Prolog."
  exit 1
fi

install="$(dirname $0)/install.pl"
$SWIPL -q -f "$install" -- $*
