import re
import sys

def my_replace(match):
  return str(ord(match.group()[2]))

print(re.sub(r"u'.'", my_replace, sys.stdin.read()))
