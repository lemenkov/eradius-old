#!/usr/bin/env python

import os

BaseDir="./priv/dictionaries/"

def make_attr_list(String):
	SplittedStr = String.split('#')[0].split()

	if SplittedStr == []:
		return []
	else:
		# AttrName cannot contain dots
		if SplittedStr[0] != '$INCLUDE':
			SplittedStr[1] = SplittedStr[1].replace('.', '')

		if len(reduce(lambda x,y: x + y, [x.split('.') for x in SplittedStr])) != len(SplittedStr):
			# Bypass floating point values
			return []
		else:
			return SplittedStr

def to_int(ProbablyInt):
	if len(ProbablyInt) > 2 and ProbablyInt[0] == '0' and ProbablyInt[1] == 'x':
		return int(ProbablyInt, 16)
	else:
		return int(ProbablyInt)

def to_atom(Name):
	return Name.replace('-', '_')

def parse(Filename):

	Vendor = {}
	VendorDefault = False

	FdIn = open(BaseDir + Filename)
	FdOut = open("./include/" + Filename.replace('.', '_') + ".hrl", 'w')
	FdOutMap = open("./priv/" + Filename.replace('.', '_') + ".map", 'w')

	line = FdIn.readline()
	while line:
		AttrList = make_attr_list(line)

		if AttrList != []:
			if AttrList[0] == 'VENDOR' and len(AttrList) > 2:
				Vendor[AttrList[1]] = int(AttrList[2])
				FdOut.write("-define( %s , %s ).\n" % (to_atom(AttrList[1]), AttrList[2]))
				FdOutMap.write("{vendor, %s, \"%s\"}.\n" % (to_atom(AttrList[2]), AttrList[1]))
			if AttrList[0] == 'BEGIN-VENDOR' and Vendor != {}:
				VendorDefault = True
			if AttrList[0] == 'END-VENDOR':
				VendorDefault = False
#			if AttrList[0] == '$INCLUDE':
#				print AttrList
#				FdOut.write("-include( \"%s\" ).\n" % (AttrList[1].replace('.', '_') + ".hrl"))
			if AttrList[0] == 'ATTRIBUTE':
				if (len(AttrList) == 4) and VendorDefault == False:
#					print "Simple case: ", AttrList, Vendor, Filename
					FdOut.write("-define( %s , %d ).\n" % (to_atom(AttrList[1]), to_int(AttrList[2])))
					FdOutMap.write("{attribute, %d, %s, \"%s\"}.\n" % (to_int(AttrList[2]), AttrList[3], to_atom(AttrList[1])))
				elif (len(AttrList) == 5) and VendorDefault == False and Vendor == {}:
#					print "Simple case with additional comment: ", AttrList, Filename
					FdOut.write("-define( %s , %d ).\n" % (to_atom(AttrList[1]), to_int(AttrList[2])))
					FdOutMap.write("{attribute, %d, %s, \"%s\"}.\n" % (to_int(AttrList[2]), AttrList[3], to_atom(AttrList[1])))
				elif (len(AttrList) == 4) and VendorDefault and Vendor != {}:
#					print "VendorDefault: ", AttrList, Vendor, Filename
					FdOut.write("-define( %s , {%s,%d} ).\n" % (to_atom(AttrList[1]), Vendor[Vendor.keys()[0]], to_int(AttrList[2]) ))
					FdOutMap.write("{attribute, {%s,%d}, %s, \"%s\"}.\n" % (Vendor[Vendor.keys()[0]], to_int(AttrList[2]), AttrList[3], to_atom(AttrList[1])))
				elif (len(AttrList) == 5) and VendorDefault and Vendor != {}:
#					print "VendorDefault with additional comment: ", AttrList, Vendor, Filename
					FdOut.write("-define( %s , {%s,%d} ).\n" % (to_atom(AttrList[1]), Vendor[Vendor.keys()[0]], to_int(AttrList[2]) ))
					FdOutMap.write("{attribute, {%s,%d}, %s, \"%s\"}.\n" % (Vendor[Vendor.keys()[0]], to_int(AttrList[2]), AttrList[3], to_atom(AttrList[1])))
				else:
					print "Unknown: ", AttrList, VendorDefault, Vendor, Filename, len(AttrList)

		line = FdIn.readline()

	FdIn.close()
	FdOut.close()
	FdOutMap.close()

DictList = os.listdir(BaseDir)
for i in DictList:
	parse(i)

