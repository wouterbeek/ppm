#!/bin/bash

function dnf_installed {
  if dnf list installed "$@" >/dev/null 2>&1; then
    true
  else
    false
  fi
}

findexe()
{
  oldifs="$IFS"
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

#if ! findexe swipl; then
  sudo dnf -q -y install \
     autoconf \
     chrpath \
     curl \
     freetype-devel \
     git \
     gmp-devel \
     java-1.8.0-openjdk-devel \
     jpackage-utils \
     libarchive-devel \
     libedit-devel \
     libICE-devel \
     libjpeg-turbo-devel \
     libSM-devel \
     libunwind \
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
     serd-devel \
     unixODBC-devel \
     uuid-devel \
     zlib-devel
  #export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.131-1.b12.fc25.x86_64/jre/lib/amd64/:/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.131-1.b12.fc25.x86_64/jre/lib/amd64/server/
  git clone https://github.com/SWI-Prolog/swipl-devel && \
    cd swipl-devel && \
    ./prepare --yes && \
    cp build.templ build && \
    ./build;
fi

setup="$(dirname $0)/setup.pl"
swipl -q -f "$setup" -- $*
