#!/usr/bin/python3
#####################################################
#############  LEAVE CODE BELOW  ALONE  #############
# Include base directory into path
import os, sys
sys.path.append(os.path.abspath(os.path.join(os.path.dirname( __file__ ), '..', '..')))

# Import tester
from tester import failtest, passtest, assertequals, runcmd, preparefile, runcmdsafe
#############    END UNTOUCHABLE CODE   #############
#####################################################

###################################
# Write your testing script below #
###################################

# prepare necessary files
preparefile('./test.rkt')
preparefile('./input')


# run test file
b_stdout, b_stderr, b_exitcode = runcmdsafe(f'racket ./test.rkt')


# convert stdout bytes to utf-8
stdout = ""
stderr = ""
try:
	stdout = b_stdout.decode('utf-8')
	stderr = b_stderr.decode('utf-8')
except:
	pass


# stdout comparison (answer == output)
try:
	with open('answer', 'r') as file1, open('output', 'r') as file2:
		answer = file1.read().strip()
		output = file2.read().strip()
	os.remove('output')
	assertequals(answer, output, stdout+"\n"+stderr)
except FileNotFoundError:
	failtest(stdout+"\n\n"+stderr)
except ValueError:
	os.remove('output')
	failtest(stdout+"\n"+stderr)
