'''
PERMAS dat parser

returns a dictionary of keywords
'''

def _strip_comments(line):
  line = line.strip()
  if line.startswith('!'):
    line = ''
  elif '!' in line:
    line = line.split('!')[0].strip()
  return line


def _strip_spaces(line):
  line = line.strip()
  while '  ' in line:
    line = line.replace('  ', ' ')
  line = line.replace(' =', '=').replace('= ', '=')
  return line


def _strip_all(line):
  return _strip_spaces(_strip_comments(line))

def get_params(line, required_keywords = None):
  params = {}
  if required_keywords is None:
    required_keywords = []
  line = _strip_all(line)
  if line == '':
    return params
  words = line.split(' ')
  params[words[0]] = None
  for w in words[1:]:
    if '=' in w:
      key = w.split('=')
      value = key[1]
      key = key[0]
      params[key] = value
    else:
      params[key] += w

  for key in params.keys():
    if "'" in params[key]:
      new_val = []
      odd = False
      for val in params[key].split("'"):
        odd = not odd
        if val == "":
          continue

    if ' ' in params[key]:
      params[key] = params[key].split(' ')
      new_val = []
      i = 0
      while i < len(params[key]):
        val = params[key][i]
        if val.startswith('"') or val.startswith("'"):
          char = '"' if val.startswith('"') else "'"
          i += 1
          val = params[key][i]
          while i <= len(params[key]):
            if i == len(params[key]):
              new_val[-1] = new_val[-1].strip(char)
              break
        else:
          new_val.append(val)



  msg = ''
  for key in required_keywords:
    if key not in params.keys():
      msg += f'{key:s} not found in {line}.\n'
  if msg:
    raise RuntimeError(msg)
  return params







