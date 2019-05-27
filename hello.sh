#!/bin/sh
set -eu
cd "$(dirname "$0")"
mkdir -p hello
cd hello
../lander.scm <../hello.lisp
ansible-playbook "$@" hello.yml
