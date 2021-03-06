#!/usr/bin/env python

# https://github.com/alfredo/local-bin/blob/master/pyflakespep8.py

import commands
import re
import sys


def make_re(*msgs):
    return re.compile('(%s)' % '|'.join(msgs))

pyflakes_ignore = make_re('unable to detect undefined names')
pyflakes_warning = make_re(
    'imported but unused',
    'is assigned to but never used',
    'redefinition of unused',
)
# E221,E701,E202
pep8_ignore = ['E202', ]
pep8_warning = make_re('.')

def run_cmd(cmd):
    import shlex
    import os
    from subprocess import Popen, PIPE, STDOUT
    args = shlex.split(cmd, posix=os.name=='posix')
    pipe = Popen(args, stdout=PIPE, stderr=STDOUT, shell=True)
    result = pipe.stdout.read()
    return result

def run(cmd, ignore_re, warning_re):
    # output = commands.getoutput(cmd)  # not work on windows
    output = run_cmd(cmd)
    for line in output.splitlines():
        if ignore_re and ignore_re.search(line):
            continue
        if ': ' in line and warning_re and warning_re.search(line):
            line = '%s: Warning %s' % tuple(line.split(': ', 1))
        print line

run('epylint %s 2>/dev/null' % sys.argv[1], None, None)
run('pyflakes %s' % sys.argv[1], pyflakes_ignore, pyflakes_warning)
pep8_ignore = ' '.join('--ignore=%s' % i for i in pep8_ignore)
run('pep8 %s --repeat %s' % (pep8_ignore, sys.argv[1]), None, pep8_warning)
