#!/usr/bin/python
import sys
import os

def make_all():
	print "making everything"
	d = "./src/"
	files = [f[:-3] for f in os.listdir(d) if os.path.isfile(d+f) and f[-3:] == ".hs" and f[:-3].isdigit() ]
	
	make(files)	

def build(p):
	print "building",p

	#first check that it doesn't exist already
	src_path = "./src/%s.hs" % (p)
	if os.path.exists(src_path):
		print "already built"
		return
	
	#program hasn't been built yet, so make a generic starter file
	contents = "module Main (main, euler%s) where\neuler%s = 0\nmain = print euler%s" % (p,p,p)
	with open(src_path,"w") as f:
		f.write(contents)


			

#compiles all programs in the list
def make(prog_list):
	for p in prog_list:
		cmd = "ghc --make src/%s.hs -outputdir=src/obj/%s -o bin/%s -i./lib:./src" % (p,p,p)
		print cmd
		os.system(cmd)
			


def main(argv):
	if len(argv) == 2 and argv[1] == "all":
		make_all()
		return

	elif len(argv) == 3 and argv[1] == "build" and argv[2].isdigit():
		build(argv[2])
		return

	elif len(argv) == 1:
		print "expecting arguments to be numbers"
		return 
	
	validation = [v.isdigit() for v in argv[1:]]
	if False in validation:
		print "expecting arguments to be numbers"

	make(argv[1:])
	return

if __name__=="__main__":
	main(sys.argv)
