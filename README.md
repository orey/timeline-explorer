# Timeline Explorer

This utility program takes as input a folder, scans it for all files and sort it from the more recent to the oldest one. It generates an html file pointing to files, sorted in a timeline manner.

Updated: December 28 2019

## Stable version

  * `champ4.lisp` : stable version
  
## Usage

Just load the file and follow instructions.

## To do

  * Compare sorting performance of `vector` compared to `list`.
  * Use macros to generate html in an easier way.
  * Optimize
  
## Implementation notes

The v4 version is not optimized:

  * The basic recursion explores the tree of folders and aggregates it in a list of files
    * Used type (string): `(namestring (car (directory XX)))`
  * Another loop creates the datetime tags and store it in a list:
    * `((second-nb1 namestring1) (second-nb2 namestring2) ... (second-nbi namestringi))`
  * Then the list is sorted based on timestamps.
  * Then the html is generated (with the index in parallel).

