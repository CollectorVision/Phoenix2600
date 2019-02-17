#!/usr/bin/python

# EP 2018-10-28
# Rename a set of files with python

import os


def dirtree():
	for dirname, dirnames, filenames in os.walk('.'):
		# print path to all subdirectories first.
		for subdirname in dirnames:
			print(os.path.join(dirname, subdirname))

		# print path to all filenames.
		for filename in filenames:
			print(os.path.join(dirname, filename))

		# Advanced usage:
		# editing the 'dirnames' list will stop os.walk() from recursing into there.
		if '.git' in dirnames:
			# don't go into any .git directories.
			dirnames.remove('.git')
        
      
print "----------"  
myfiles = filter(lambda x: os.path.isfile(x), os.listdir('.'))

#name = '.'
source_name = "zxuno_a2601"
dest_name = "pepino"

print source_name, len(source_name)

print myfiles
for x in myfiles:
	y = x.split('.')
	if y[0] == source_name[:len(source_name)] :
		# print y[0], y[1]
		# print "Renaming {0}.{1} to {2}.{1}".format(y[0], y[1], dest_name)
		os.rename("{0}.{1}".format(y[0], y[1]), "{0}.{1}".format(dest_name, y[1]))

