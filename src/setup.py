#!/usr/bin/python
import os, os.path

#Check if a directory is ok
def okLibDir(dir):
	return (dir.find(".svn") == -1 and dir.find("Clean System Files") == -1 and dir.find("OS-Windows") == -1)

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

