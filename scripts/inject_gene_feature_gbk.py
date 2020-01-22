#!/usr/bin/env python
import argparse
import os
import re

def main():
    # store commandline args
    parser = argparse.ArgumentParser(description='Injects gene features for CDS features without parent files.')
    parser.add_argument("--in_filepath", help='Input filepath', required=True)
    parser.add_argument("--out_filepath", help='Path to write output')
    
    args = parser.parse_args()   
    lines = open(args.in_filepath)
    newCDS = 0
    smORFLine = ""
    cdsLines = []
    geneLines = []
    for line in lines:
      #Flush condition
      if "     ncRNA" in line:
         flush(geneLines,cdsLines)
         newCDS = 0
         cdsLines = []
         geneLines = []
         smORF = 0
      if "     misc_binding" in line:
         flush(geneLines,cdsLines)
         newCDS = 0
         cdsLines = []
         geneLines = []         
         smORF = 0
      if "     misc_feature" in line:
         flush(geneLines,cdsLines)
         newCDS = 0
         cdsLines = []
         geneLines = []         
         smORF = 0
      if "     gene" in line:
         flush(geneLines,cdsLines)
         newCDS = 0
         cdsLines = []
         geneLines = []         
         smORF = 0
      if "     CDS" in line:
         flush(geneLines,cdsLines)
         newCDS = 1
         #print("newCDS")
         cdsLines = []
         geneLines = []         
         smORF = 0
         geneLinesHeader = line
      if newCDS == 1:
          if "smORF" in line:
            #print("smORF detected")
            smORF=1
            smORFLine = line
            locusTagLine=line
            locusTagLine=locusTagLine.replace("label","locus_tag")
            genesmORFLine=line
            genelocusTag=locusTagLine
            cdsLines.append(smORFLine)
            cdsLines.append(locusTagLine)
            geneLines.append(geneLinesHeader.replace("CDS ","gene"))
            geneLinesHeader=""
            geneLines.append(smORFLine)
            geneLines.append(locusTagLine)
            #geneline = re.sub(r'Parent=\w+;', '', geneline)
            #print(geneline, end = '')
            #print(line, end = '')
          else:
            cdsLines.append(line)
      else:
          print(line,end = '')
    flush(geneLines,cdsLines)

def flush(cdsLines,geneLines):
    print(*cdsLines, sep = "", end = '')
    print(*geneLines, sep = "",end = '')

        
if __name__ == '__main__':
    main()
