#!/bin/sh
set -eu
cd "$(dirname "$0")"
mkdir -p hello
cd hello
gosh ../lander.scm <../hello.lisp
ansible-playbook "$@" hello.yml
