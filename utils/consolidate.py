import os
import sys


def is_source_file(f):
	return f[-3:] == ".hs"

def main():
	dirs = ["./"+d for d in os.listdir("./") if os.path.isdir(d) and d != "lib"]
				
	files = [d+"/"+f for d in dirs for f in os.listdir(d) if is_source_file(f)]
	
	#copy files over into src dir
	for f in files:
		cmd = "cp %s ./src/" % (f)
		os.system(cmd)

if __name__=="__main__":main()
