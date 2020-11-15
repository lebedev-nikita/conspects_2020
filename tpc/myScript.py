from os import listdir
import json
import solution

sol = solution.Solution()

def readFile(fileName):
  f = open(fileName, 'r')
  s = f.read()
  f.close()
  return s

path = r'./train/txts/'
docNames = listdir(path)
docPaths = list(map(lambda name: path + name, docNames))
fileStrings = list(map(readFile, docPaths))

# b = sol.predict(fileStrings[0:])
b = sol.predict(fileStrings[0:])
print(json.dumps(b, indent=2, ensure_ascii=False))



