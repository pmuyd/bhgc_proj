#!/bin/bash

# dependencies
sudo apt-get update
sudo apt-get install -y $(cat requirements.txt)

# valgrind for memory analysis
valgrind --leak-check=full --show-leak-kinds=all python3 tester.py -a