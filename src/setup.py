#!/usr/bin/python
import os, os.path, sys, math

#Determine if we are on Linux or OS X
uname = os.uname()[0]
is64 = (math.log(sys.maxsize,2) > 32)

if uname == 'Darwin':
	usedirs = ['OS-Independent','OS-Posix','OS-Mac']
if uname == 'Linux':
	if is64:
		usedirs = ['OS-Independent','OS-Posix','OS-Linux','OS-Linux-64']
	else:
		usedirs = ['OS-Independent','OS-Posix','OS-Linux','OS-Linux-32']

#Check if a directory is ok
def okLibDir(dir):
	if dir.find(".svn") != -1:
		return False
	if dir.find("Clean System Files") != -1:
		return False
	if(dir.find("OS-") != 0):
		for allowed in usedirs:
			if dir.find(allowed) != -1:
				return True

		return False
	
	return True

#Create dirs file
print ("Creating env/Clean Platform.dirs...")

dirlist = ""
for (dir,subdirs,files) in os.walk("libraries"):
	if okLibDir(dir):
		dirlist = dirlist + os.path.realpath(dir) + ":"
dirlist = dirlist + "."
dirfile = open("env/Clean Platform.dirs","w")	
dirfile.write(dirlist)
dirfile.close()

#Create aggregated libs
print("Creating env/all-libraries...")

os.system("mkdir -p env/all-libraries")

for (dir,subdirs,files) in os.walk("libraries"):
	if okLibDir(dir):
		for file in files:
			os.system("ln -f -s ../../%s/%s env/all-libraries/%s" % (dir,file,file))

