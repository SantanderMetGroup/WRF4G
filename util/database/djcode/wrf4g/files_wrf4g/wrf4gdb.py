#! /usr/bin/env python
import sys

class WRF4GDB:
  def __init__(self):
    pass
  def mimetodo(self, arg="ninguno"):
    print "Me has llamado: mimetodo %s" % arg

if __name__ == "__main__":
  w4gdb = WRF4GDB()
  getattr(w4gdb, sys.argv[1])(*sys.argv[2:])
