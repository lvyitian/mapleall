#
# Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
#
# OpenArkCompiler is licensed under the Mulan PSL v1.
# You can use this software according to the terms and conditions of the Mulan PSL v1.
# You may obtain a copy of Mulan PSL v1 at:
#
#     http://license.coscl.org.cn/MulanPSL
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
# FIT FOR A PARTICULAR PURPOSE.
# See the Mulan PSL v1 for more details.
#
#open(my $aarch64_isa_def_h, '<',  "aarch64_isa.def.in") or die "can't open aarch64_isa.def.in";
#open(my $aarch64_isa_def, '>',  "aarch64_isa.def") or die "can't open aarch64_isa.def";
#print $aarch64_isa_def "\/\/Do not modify this file manually\n";
#my $insn_count=1;
#while(my $line = <$aarch64_isa_def_h>) {
#  if ($line =~ /\/\//){
#    next;
#  }
#  elsif ($line =~ /( )*MOP_/){
#    $line =~ s/( )*MOP_/\#define MOP_/;
#    $line =~ s/,/   $insn_count/;
#    $insn_count++;
#  }else {
#    next;
#  }
#  print $aarch64_isa_def $line;
#}
#print $aarch64_isa_def "\#define MOP_last ".$insn_count."\n";
#close $aarch64_isa_def;
#close $aarch64_isa_def_h;

import sys
#import getopt

''' read line {...} and process it '''

#def readmore(infile):
#  return infile.read(4096)

def check(line):
  n = len(line)
  if line[0] != '{':
    raise Exception("Should begin with '{'");
  if line[n-1] != ',':
    raise Exception("Record delimitter is missing");

  s = 0
  for i in range(0,n):
    c = line[i]
    if c == '{':
       s += 1
    elif c == '}':
       s -= 1

    if s < 0:
       raise Exception("Unmatched '}'")
  if s > 0:
    raise Exception("Unmatched '{'")

def process_line(line):
  line = line.strip()
  if len(line) == 0:
    return

  if line[0] == '/' and line[1] == '/':
    # comment
    return

  try:
    check(line)
  except:
    print "'"+line+"' is invalid"
    return

  k = line.find(',',0)
  key = line[1:k]
  print key.strip() + ","

def process(mdfilename):
  with open( mdfilename, "r" ) as infile:
    for l in infile:
      process_line(l)

def help():
  print "Usage: " + sys.argv[0] + " md-file"

def main():
  # try:
  #  opts, args = getopt.getoopt(sys.argv[1:], "h", ["help"])
  #except getopt.error, msg:
  #  print msg
  #  print "for help, use --help"
  #  sys.exit(-1)
  if len(sys.argv) != 2:
    help()
    return

  process(sys.argv[1])

  #for o, a in opts:
  #  if o in ("-h","--help"):
  #    help()
  #    sys.exit(0)

  #for arg in args:
  #  process(arg)

if __name__ == "__main__":
  main()
