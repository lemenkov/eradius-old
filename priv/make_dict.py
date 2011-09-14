#!/usr/bin/env python

import getopt, os, sys

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
		else:
			return SplittedStr

def to_int(ProbablyInt):
	if len(ProbablyInt) > 2 and ProbablyInt[0] == '0' and ProbablyInt[1] == 'x':
		return int(ProbablyInt, 16)
	else:
		return int(ProbablyInt)

def to_atom(Name):
	if Name[0] in "0123456789":
		N = "'" + Name + "'"
	else:
		N = Name
	return N.replace('-', '_').replace('/', '_').replace('+','plus').replace(',','_')

def from_atom(Atom):
	return Atom.replace("'", "")

def parse(Filename):

	Vendor = {}
	Includes = []
	Attrs = {}
	Vals = {}

	VendorDefault = False

	FdIn = open(Filename)

	line = FdIn.readline()
	while line:
		AttrList = make_attr_list(line)

		if AttrList != []:
			if AttrList[0] == 'VENDOR' and len(AttrList) > 2:
				Vendor[to_atom(AttrList[1])] = int(AttrList[2])
			elif AttrList[0] == 'BEGIN-VENDOR' and Vendor != {}:
				VendorDefault = True
			elif AttrList[0] == 'END-VENDOR':
				VendorDefault = False
			elif AttrList[0] == '$INCLUDE':
				Includes += [AttrList[1].replace('.', '_') + ".hrl"]
			elif AttrList[0] == 'ATTRIBUTE':
				if (len(AttrList) == 4) and VendorDefault == False:
					Attrs[to_atom(AttrList[1])] = (AttrList[3], to_int(AttrList[2]))
				elif (len(AttrList) == 5) and VendorDefault == False and Vendor == {}:
					Attrs[to_atom(AttrList[1])] = (AttrList[3], to_int(AttrList[2]))
				elif (len(AttrList) == 4) and VendorDefault and Vendor != {}:
					Attrs[to_atom(AttrList[1])] = (AttrList[3], (Vendor[Vendor.keys()[0]],  to_int(AttrList[2])))
				elif (len(AttrList) == 5) and VendorDefault and Vendor != {}:
					Attrs[to_atom(AttrList[1])] = (AttrList[3], (Vendor[Vendor.keys()[0]],  to_int(AttrList[2])))
				else:
					print "Unknown AttrList layout: ", AttrList, VendorDefault, Vendor, Filename, len(AttrList)
			elif AttrList[0] == 'VALUE':
				if (len(AttrList) == 4):
					Vals[ (to_atom(AttrList[1]), to_atom(AttrList[2])) ] = to_int(AttrList[3])
				else:
					print "Unknown Value layout: ", AttrList
			else:
				print "Unknown Type: ", AttrList, Filename

		line = FdIn.readline()

	FdIn.close()

	return Vendor, Vals, Attrs, Includes

##
## Main function
##

if __name__ == '__main__':
	try:
		opts, args = getopt.getopt(sys.argv[1:], 'f:d:i:m:')
	except getopt.GetoptError:
		print('usage: b2bua.py [-f filename] [-d basedir] [-i includedir] [-m mapdir]')
		sys.exit(1)

	Filename = ""
	BaseDir = ""
	IncDir = "./"
	MapDir = "./"
	DictList = []

	for o, a in opts:
		if o == '-f':
			Filename = a
			BaseDir = os.path.dirname(Filename) + "/"
			DictList = [os.path.basename(Filename)]
			continue
		if o == '-d':
			BaseDir = a + "/"
			DictList = sorted(os.listdir(BaseDir))
			continue
		if o == '-i':
			IncDir = a + "/"
			continue
		if o == '-m':
			MapDir = a + "/"
			continue

	if Filename == "" and BaseDir == "":
		print('usage: b2bua.py [-f filename] [-d basedir] [-i includedir] [-m mapdir]')
		print('you must specify either base directory or particular file to process')
		sys.exit(1)

	AllVendors = {}

	for Filename in DictList:
		Vendors, Vals, Attrs, Includes = parse(BaseDir + Filename)

		FdOut = open(IncDir + Filename.replace('.', '_') + ".hrl", 'w')
		FdOutMap = open(MapDir + Filename.replace('.', '_') + ".map", 'w')

		# Add guarding header to prevent multiple inclusion
		FdOut.write("-ifndef(_%s_INCLUDED).\n" % Filename.upper().replace('.', '_'))
		FdOut.write("-define(_%s_INCLUDED, true).\n\n" % Filename.upper().replace('.', '_'))

		# Print includes
		for Include in Includes:
			FdOut.write("-include( \"%s\" ).\n" % (Include))

		# Print vendor
		for Vendor in Vendors.keys():
			if Vendor in AllVendors.keys():
				# FIXME add necessary -include() ?
				print "Vendor %s already defined somewhere else" % Vendor
			else:
				FdOut.write("-define( %s , %s ).\n" % (Vendor, Vendors[Vendor]))
				FdOutMap.write("{vendor, %s, \"%s\"}.\n" % (Vendor, Vendors[Vendor]))

		AllVendors.update(Vendors)

		# Print attrs
		for Attr in Attrs.keys():
			(Type, Val) = Attrs[Attr]
			if isinstance(Val, tuple):
				SubType, SubVal = Val
				FdOut.write("-define( %s , {%s,%s} ).\n" % (Attr, SubType, SubVal))
				FdOutMap.write("{attribute, {%s,%s}, %s, \"%s\"}.\n" % (SubType, SubVal, Type, Attr))
			else:
				FdOut.write("-define( %s , %s ).\n" % (Attr, Val))
				FdOutMap.write("{attribute, %s, %s, \"%s\"}.\n" % (Val, Type, Attr))

		# Print values
		for Val in Vals.keys():
			(Parent, Name) = Val

			FdOut.write("-define( Val_%s_%s , %s ).\n" % (from_atom(Parent), from_atom(Name), Vals[Val]))
			# FIXME
			FdOutMap.write("{value, %s, \"%s\"}.\n" % (Vals[Val], from_atom(Name)))

		FdOut.write("\n-endif.\n")

		FdOut.close()
		FdOutMap.close()
