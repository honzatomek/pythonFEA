import os
import sys
import re
import logging
import inspect

logging.addLevelName(logging.INFO, 'COMMENT') # rename INFO to COMMENT
BASIC_FORMAT = logging.Formatter('%(message)s')
ADVANCED_FORMAT = logging.Formatter('%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')
logging.basicConfig(level=logging.DEBUG, format='%(asctime)6s *%(levelname).1s* %(message)s', datefmt='%H%M%S')


#--------------------------------------------------------------------------------------- CONSTANTS ---# {{{1
DOFTYPES = ['DISP']


#-------------------------------------------------------------------------------------- DECORATORS ---# {{{1
class hybridmethod:
  '''
  class decorator for a classmethod with optional instance method

  From:
    https://stackoverflow.com/questions/28237955/same-name-for-classmethod-and-instancemethod
  Use:
    class Foo:
      @hybridmethod
      def bar(cls, *args, **kwargs):
        print(f'Bound to the class, {cls}.')

      @bar.instancemethod
      def bar(self, *args, **kwargs):
        print(f'Bound to the instance, {self}.')

    Foo.bar()   # classmethod
    Foo().bar() # instancemethod
    baz = Foo() # create instance
    baz.bar()   # instancemethod
  '''
  def __init__(self, fclass, finstance=None, doc=None):
    # print('hybridmethod.__init__()')
    self.fclass = fclass
    self.finstance = finstance
    self.__doc__ = doc or fclass.__doc__
    # support use on abstract base classes
    self.__isabstractmethod__ = bool(getattr(fclass, '__isabstractmethod__', False))

  def classmethod(self, fclass):
    # print('hybridmethod.classmethod()')
    return type(self)(fclass, self.finstance, None)

  def instancemethod(self, finstance):
    # print('hybridmethod.instancemethod()')
    return type(self)(self.fclass, finstance, self.__doc__)

  def __get__(self, instance, cls):
    # print('hybridmethod.__get__()')
    if instance is None or self.finstance is None:
      # either bound to the class, or no instance method available
      return self.fclass.__get__(cls, None)
    return self.finstance.__get__(instance, cls)


def error_decorator(exception):
  def wrapper(*args, **kwargs):
    logging.error(args[0])
    return exception(*args, **kwargs)
  return wrapper


class logging_decorator:
  __indent = 0

  def __init__(self, Log):
    self.log = Log
    # support use on abstract base classes
    self.__isabstractmethod__ = bool(getattr(Log, '__isabstractmethod__', False))

  def __call__(self, *args, **kwargs):
    args = list(args)
    # if '<<<' in log string - return from resource -> dedent this and following logs
    if '<<<' in args[0]:
      self.dedent()
    # indent multiline log entries
    if '\n' in args[0]:
      args[0] = '\n'.join([' ' * 11 + a if a != '' and i > 0 else a for i, a in enumerate(args[0].split('\n'))])
    if type(self).__indent > 0:
      args[0] = indent(args[0], type(self).__indent)
    # if '>>>' in log string - go into a resource -> indent next and following logs
    if '>>>' in args[0]:
      self.indent()
    return self.log(*args, **kwargs)

  def indent(self):
    type(self).__indent += 2

  def dedent(self):
    type(self).__indent = max(type(self).__indent - 2, 0)


logging.debug = logging_decorator(logging.debug)
logging.info = logging_decorator(logging.info)
logging.warning = logging_decorator(logging.warning)
logging.error = logging_decorator(logging.error)
logging.exception = logging_decorator(logging.exception)


#------------------------------------------------------------------------------------------ ERRORS ---# {{{1
@error_decorator
class ParseError(ValueError):
  pass


@error_decorator
class ReadError(ValueError):
  pass


@error_decorator
class ValueError(ValueError):
  pass


@error_decorator
class TypeError(TypeError):
  pass


@error_decorator
class AttributeError(AttributeError):
  pass


@error_decorator
class OverflowError(OverflowError):
  pass


@error_decorator
class NotImplementedError(NotImplementedError):
  pass


#---------------------------------------------------------------------------------- HELP FUNCTIONS ---# {{{1
def comment(text: str, commant_char: str = '! ') -> str:
  return '\n'.join([comment_char + line for line in text.split('\n')])


def indent(text: str, indent_level: int = 2) -> str:
  return '\n'.join([' ' * indent_level + line for line in text.split('\n')])


def indent_without_comment(text: str, indent_level: int = 2, comment_char = '!') -> str:
  return '\n'.join([' ' * indent_level + line if not line.startswith(comment_char) else line for line in text.split('\n')])


def single_spaces(text: str) -> str:
  while '  ' in text:
    text = text.replace('  ', ' ')
  return text


def format_description(description: list) -> str:
  if type(description) is str:
    description = description.split('\n')

  for i in range(len(description)):
    description[i] = description[i].rstrip().rstrip('! ').rstrip('!')

  return '\n'.join(description)


def get_name(line: str):
  orig_line = line
  line = single_spaces(line.upper()).strip()
  name = ''
  if 'NAME = ' in line:
    name = line.split('NAME = ')[1].strip().split(' ')[0]
  elif 'NAME=' in line:
    name = line.split('NAME=')[1].strip().split(' ')[0]
  else:
    name = ''

  if len(name) > 0:
    j = orig_line.upper().find(name)
    l = len(name)
    name = text[i][j:j+l]

  return name


def get_doftype(line: str):
  doftypes = list()
  if 'DOFTYPE = ' in line.upper() or 'DOFTYPE=' in line.upper():
    for dt in DOFTYPES:
      if dt in line.upper():
        doftypes.append(dt)
  return doftypes


def format_eng(value, format_spec: str = ' {0:9.3f}E{1:+03n}') -> str:
    try:
        if value == 0.0:
            return format_spec.format(0.0, 0)
        exponent = int(math.log10(abs(value)))
        exponent = exponent - exponent % 3
        mantissa = value / (10 ** exponent)
        return format_spec.format(mantissa, exponent)
    except OverflowError as e:
        logging.exception(e)
        return str('{0:' + str(len(format_eng(1.1, format_spec))) + 'n}').format(np.infty)
    except ValueError as e:
        logging.exception(e)
        return str('{0:' + str(len(format_eng(1.1, format_spec))) + 'n}').format(np.nan)


def err_msg(msg, text: list, line: int, find: str = None, numlines: int = 1) -> str:
  if text is not None:
    if line is None:
      raise ValueError(f'err_msg() must be supplied with line if text is supllied.')

    for i in range(numlines):
      if line - numlines + i >= 0:
        msg += '\n' + text[line - numlines + i]

    msg += '\n' + text[line]
    if find is not None:
      msg += '\n' + ' ' * text[line].upper().find(find) + '^'

    for i in range(numlines):
      if line + i + 1 <= len(text):
        msg += '\n' + text[line + i + 1]

  return msg



#-------------------------------------------------------------------------------------- DAT PARSER ---# {{{1
class DATFile:
  def __init__(self, filename: str, encoding: str = 'utf-8'):
    if os.path.isfile(filename):
      self.filename = filename
    else:
      raise ParseError(f'{type(self).__name__:s}: filename {filename:s} is unreachable.')
    self.encoding = encoding

    self.file = None
    self.linenumber = 0
    self.line = ''
    self.eof = False

  def __del__(self):
    try:
      self.file.close()
    except:
      pass

  def __next__(self):
    if self.file is None:
      self.linenumber = 0
      self.file = open(self.filename, 'r', encoding=self.encoding)
      # logging.info(f'>>> Opened {self.filename}.')

    line = self.file.readline()
    if not line:
      self.eof = True
      self.file.close()
      self.file = None
      # logging.info(f'>>> Closed {self.filename}.')
      return None
    else:
      self.linenumber += 1
      self.line = line.strip('\n')
      return self.line

  def __iter__(self):
    return self


class DATBuffer:
  def __init__(self, buffsize: int = 5):
    self.buffsize = int(buffsize / 2) * 2 + 1
    self.line_buffer = list()
    self.line_number = list()
    self.buffline = -1 - int(self.buffsize / 2)

  def __str__(self):
    return '\n'.join([f'{self.line_number[i]:10n}: {self.line_buffer[i]:s}' for i in range(len(self.line_buffer))])

  def __iter__(self):
    return iter([(self.line_number[i], self.line_buffer[i]) for i in range(len(self.line_buffer))])

  def __len__(self):
    return len(self.line_buffer)

  def add(self, line, line_number):
    self.buffline += 1
    if line is not None:
      self.line_buffer.append(line)
      self.line_number.append(line_number)
    else:
      self.buffline -= 1
      self.line_buffer.pop(0)
      self.line_number.pop(0)

    if len(self.line_buffer) > self.buffsize:
      self.buffline -= 1
      self.line_buffer.pop(0)
      self.line_number.pop(0)

  @property
  def line(self):
    if self.buffline < len(self.line_buffer) and self.buffline >= 0:
      return self.line_buffer[self.buffline]
    else:
      return None

  @property
  def lineno(self):
    if self.buffline < len(self.line_buffer) and self.buffline >= 0:
      return self.line_number[self.buffline]
    else:
      return None

  @property
  def end(self):
    return self.buffline == len(self.line_buffer) - 1


class DATReader:
  def __init__(self, filename: str, encoding: str = 'utf-8', buffsize: int = 5):
    self.file = DATFile(filename, encoding)
    self.buffer = DATBuffer(buffsize)
    self.err_msg = ''
    self.eof = False
    self.read = False

  def __init(self):
    self.file = DATFile(self.file.filename, self.file.encoding)
    self.buffer = DATBuffer(self.buffer.buffsize)
    self.err_msg = ''
    self.eof = False
    self.read = False

  def read_line(self):
    if self.read:
      self.__init()
      return None

    if self.buffer.line is None:
      while self.buffer.line is None:
        line = next(self.file)
        self.buffer.add(line, self.file.linenumber)
    else:
      if self.eof:
        self.buffer.add(None, None)
      else:
        line = next(self.file)
        self.buffer.add(line, self.file.linenumber)
        if line is None and self.file.eof:
          self.eof = True

    self.read = self.eof and self.buffer.end
    if self.read and self.err_msg != '':
      raise ParseError(self.err_msg)

    return self.buffer.line

  def add_error(self, msg, find: str = None, include_buffer: bool = False):
    if self.err_msg == '':
      self.err_msg = f'Parsing errors:'

    self.err_msg += f'\n- {msg} on line {self.buffer.lineno}'

    for i, line in enumerate(self.buffer):
      if i == self.buffer.buffline or include_buffer:
        self.err_msg += f'\n{line[0]:8n}: {line[1]:s}'
      if i == self.buffer.buffline:
        self.err_msg += '\n' + ' ' * 10 + ' ' * line[1].upper().find(find.upper()) + '^'

  def check_regex(self, regex):
    r = re.compile(regex, re.I)
    m = r.search(self.buffer.line)
    if m:
      return True
    else:
      return False

  def check_regex_groups(self, regex):
    r = re.compile(regex, re.I)
    m = r.search(self.buffer.line)
    if m:
      return m.group
    else:
      return None


def DATParser:
  re_component = re.compile(r"\$enter\s+component((?P<name_group>\s+name\s*=\s*(?P<name>(['\"]\S+['\"]|\w+)))|(?P<doftype_group>\s+doftype\s*=\s*(?P<doftype>(\s?(disp|pres|pote|math|temp)+)+)))+", re.I)



#-------------------------------------------------------------------------------------- BASE CLASS ---# {{{1
class pyFEA:
  def __init__(self):
    self.__parent = None

  def __getattribute__(self, name):
    returned = object.__getattribute__(self, name)
    if inspect.isfunction(returned) or inspect.ismethod(returned) or inspect.isroutine(returned):
      logging.debug(f'{type(self).__name__:s}.{returned.__name__}()')
    return returned

  @property
  def parent(self):
    return self.__parent

  @parent.setter
  def parent(self, parent):
    if not isinstance(parent, type(self)):
      raise ValueError(f'{type(self).begin:s}{type(self).command:s} parent must be an instance of Command class, not {type(parent).__name__:s}.')
    else:
      raise NotImplementedError(f'{type(self).__name__:s}.parent is not yet implemented!')



#----------------------------------------------------------------------------------------- COMMAND ---# {{{1
class Command(pyFEA):
  begin = '$COMMAND'
  command = 'COMMAND'
  end = ''
  type = 'COMMAND'

  @hybridmethod
  def read(cls, string: str, line_start: int = 0):
    print(string)
    if type(string) is not str:
      raise ValueError(f'{cls.__name__:s}.read() must be supplied with str, not {type(string).__name__:s}.')
    else:
      raise NotImplementedError(f'{cls.__name__:s}.read() method is not yet implemented!')

  @read.instancemethod
  def read(self, string: str, line_start: int = 0):
    print(string)
    if type(string) is not str:
      raise ValueError(f'{type(self).__name__:s}.read() must be supplied with str, not {type(string).__name__:s}.')
    else:
      raise NotImplementedError(f'{type(self).__name__:s}.read() method is not yet implemented!')

  def __init__(self, id, description: str = None):
    super().__init__()
    self._id = id
    self.description = description

  def __str__(self) -> str:
    out = ''
    if self.description is not None:
      out += comment(self.description)

    if type(self).begin != '':
      if out != '':
        out += '\n'
      out += f'{type(self).begin:s}'

      if type(self._id) is int:
        out += f' ID = {self._id:n}'
      else:
        out += f' NAME = {self._id:n}'

    if type(self).end != '':
      print('end')
      out += f'\n{type(self).end:s}'
    return out

  def __repr__(self) -> str:
    if type(self._id) is int:
      return f'{type(self).begin:s} ID = {self._id:n}'
    elif type(self._id) is str:
      return f'{type(self).begin:s} NAME = {self._id:s}'

  @property
  def _id(self):
    return self.__id

  @_id.setter
  def _id(self, id):
    if type(id) is int:
      self.__id = id
    elif type(id) is str:
      self.__id = id
    else:
      raise ValueError(f'{type(self).begin:s}{type(self).command:s} ID or Name must be either int or str, not {type(id).__name__:s}')

  @property
  def description(self) -> str:
    return self.__description

  @description.setter
  def description(self, description: str):
    if description is None:
      self.__description = None
    elif type(description) in [str, list]:
      self.__description = format_description(description)
    else:
      raise ValueError(f'{type(self).begin:s}{type(self).command:s} description must be a str, not {type(description).__name__:s}.')

  def draw(self, render):
    raise NotImplementedError(f'{type(self).__name__:s}.draw() method is not yet implemented!')

  def consolidate(self):
    raise NotImplementedError(f'{type(self).__name__:s}.draw() method is not yet implemented!')


#----------------------------------------------------------------------------------------- VARIANT ---# {{{1
class Variant(Command):
  begin = '$VARIANT'
  command = 'VARIANT'
  end = '$END VARIANT'
  type = 'VARIANT'

  def __init__(self, name: str, description: str = None):
    if type(name) is not str:
      raise ValueError(f'{type(self).begin:s}{type(self).command:s} NAME must be a str, not {type(name).__name__:s}.')

    super().__init__(id=name, description=description)
    self.__commands = dict()

  def __str__(self) -> str:
    out = ''

    if self.description is not None:
      out += comment(self.description)

    if type(self).begin != '':
      if out != '':
        out += '\n'
      out += f'{type(self).begin:s}'

    def recurse(cmds):
      out = ''
      if type(cmds) is dict:
        for k, c in sorted(cmds.items(), key=lambda k: k[0]):
          out += recurse(c)
      elif type(cmds) is list:
        for c in sorted(cmds, key=lambda c: c._id):
          out += recurse(c)
      else:
        out += '\n' + str(cmds) + '\n!'
      return out

    if len(self.commands.keys()) > 0:
      out += indent_without_comment(recurse(self.commands))

    if type(self).end != '':
      out += f'\n{type(self).end:s}'

    return out

  def __getitem__(self, key):
    return self.__commands[key]

  def __iter__(self):
    return iter(self.__commands)

  @property
  def commands(self) -> dict:
    return self.__commands

  def add(self, command):
    if type(command) in [list, tuple]:
      for c in command:
        self.add(c)
    elif type(command) is dict:
      for k, c in command.items():
        self.add(c)
    else:
      if not isinstance(command, Command):
        raise ValueError(f'{repr(self):s} command must be an instance if Command class, not {type(command).__name__:s}.')
      else:
        self.__commands.setdefault(command.type, dict())
        if command._id not in self.__commands[command.type].keys():
          self.__commands[command.type][command._id] = command
        else:
          raise ValueError(f'{repr(self):s} already contains {repr(command):s}.')

  def items(self) -> tuple:
    return self.__commands.items()

  def keys(self) -> list:
    return self.__commands.keys()

  def values(self) -> list:
    return self.__commands.values()

  @property
  def name(self) -> str:
    return self._id

  @name.setter
  def name(self, name: str):
    if type(name) is not str:
      raise ValueError(f'{type(self).begin:s}{type(self).command:s} NAME must be a str, not {type(name).__name__:s}.')
    else:
      self._id = name

  def draw(self, render):
    for k, commands in self.items():
      for c in commands.values():
        c.draw(render)

  def consolidate(self):
    for k, commands in self.items():
      for c in commands.values():
        c.consolidate()


#------------------------------------------------------------------------------------------- MODEL ---# {{{1
class Model(pyFEA):
  def __init__(self):
    super().__init__()
    self.name = 'TMP'
    self.__component = None
    self.__material = None
    self.__property = None

  def __str__(self) -> str:
    out = ''
    if self.component is not None:
      for cn, kompo in self.component.items():
        if out != '':
          out += '\n!\n'
        out += str(kompo)

    if self.materials is not None:
      if out != '':
        out += '\n!\n'
      out += str(self.materials)

    if self.properties is not None:
      if out != '':
        out += '\n!\n'
      out += str(self.properties)

    out += '\n!\n$FIN'

    return out

  def __repr__(self) -> str:
    return f'FEA Model NAME = {self.name:s}'

  def add(self, data_block, data_type: str = None, name: str = None, line_start: int = 0):
    if isinstance(data_block, Component):
      self.component = data_block
    elif type(data_block) is list and all([type(line) is str for line in data_block]):
      if data_type is None:
        raise AttributeError(f'{self.type:s}.add() DATA type must be specified for input.')
      elif type(data_type) is not str:
        raise TypeError(f'{self.type:s}.add() DATA type must be a str [COMPONENT, MATERIAL, PROPERTY], not {type(data_type):s}.')
      elif data_type == 'COMPONENT':
        if type(name) is not str:
          raise TypeError(f'{self.type:s}.add() COMPONENT name must be a str, not {type(name):s}.')
        else:
          if type(self.component) is dict and name in self.component.keys():
            self.component[name].read(data_block, line_start)

  @property
  def name(self) -> str:
    return self.__name

  @name.setter
  def name(self, name: str):
    if type(name) is str:
      self.__name = name
    else:
      raise ValueError(f'{type(self).__name__:s} name must be a str, not {type(name).__name__:s}.')

  @property
  def component(self):
    return self.__component

  @component.setter
  def component(self, component):
    self.__component.setdefault(component.name, component)

  @property
  def materials(self):
    return self.__material

  @materials.setter
  def materials(self, materials):
    self.__material = materials

  @property
  def properties(self):
    return self.__property

  @properties.setter
  def properties(self, properties):
    self.__property = properties

  def read_file(self, datfile: str):
    logging.info(f'>>> opening file: {datfile:s}.')
    if not os.path.isfile(datfile):
      raise ValueError(f'{datfile:s} does not exist!')
    else:
      with open(datfile, 'r', encoding='utf-8') as df:
        self.read(text=df.read())

    logging.info(f'<<< closing file: {datfile:s}.')

  def read(self, text: list):
    r = {'COMPONENT': {'start': ['$ENTER COMPONENT', r'\$ENTER\s+COMPONENT'],
                       'stop':  ['$EXIT COMPONENT',  r'\$EXIT\s+COMPONENT']},
         'MATERIAL':  {'start': ['$ENTER MATERIAL',  r'\$ENTER\s+MATERIAL'],
                       'stop':  ['$EXIT MATERIAL',   r'\$EXIT\s+MATERIAL']},
         'PROPERTY':  {'start': ['$ENTER PROPERTY',  r'\$ENTER\s+PROPERTY'],
                       'stop':  ['$EXIT PROPERTY',   r'\$EXIT\s+PROPERTY'],},}
    dp = DATParser(regex_start_stop=r, regex_error=[['$', r'\$.*']], end_offset=0, regex_stop=['$FIN', '\$FIN'], comment_str='!')

    for data_type, start, stop in dp.parse(text):
      logging.info('$ENTER ' + data_type + f' lines {start:n} - {stop:n}.')


    return

    if type(text) is not list and type(text) is str:
      text = text.split('\n')
    else:
      raise ValueError(f'{type(self).__name__:s}.read(): text must be a list of str or a str, not {type(text).__name__:s}.')

    start = -1
    end = -1
    i = -1
    read = 0
    while i <= len(text) - 1:
      i += 1
      line = single_spaces(text[i].upper()).strip()

      if line.startswith('!') or line == '':
        continue

      elif line.startswith('$ENTER'):
        logging.info(f'$ENTER {data_type}{" NAME = " + name if name != "" else "":s}')
        start = i if start == -1 else start
        name = get_name(text[i])
        data_type = line.split(' ')[1]

        if data_type not in ['COMPONENT', 'MATERIAL', 'PROPERTY']:
          raise ReadError(err_msg(f'Error in dat file on line {i + 1:n}/{len(text):n}, {data_type:s} keyword not applicable.', text, i, data_type, 2))

        for j in range(i + 1, len(text)):
          line = single_spaces(text[i].upper()).strip()

          if line.startswith('$ENTER'):
            raise ReadError(err_msg(f'Error in dat file on line {j + 1:n}/{len(text):n}, $EXIT {data_type:s} expected, found $ENTER {line.split(" ")[1]:s}.', text, j, '$ENTER', 2))

          elif line.startswith('$EXIT'):
            if data_type not in line:
              raise ReadError(err_msg(f'Error in dat file on line {j + 1:n}/{len(text):n}, $EXIT {data_type:s} expected, found $EXIT {line.split(" ")[1]:s}.', text, j, line.split(" ")[1], 2))
            end = j
            i = j
            break

        # add COMPONENT, MATERIAL, PROPERTY
        if data_type == 'COMPONENT':
          if name in self.component.keys():
            self.component[name].read(text[start:end + 1], start)
          else:
            self.component = Component.read(text[start:end + 1], start)

        logging.info(f'$EXIT {data_type}{" NAME = " + name if name != "" else "":s}')
        read += 1

      elif line.startswith('$FIN'):
        break

      elif line.startswith('$'):
        raise ReadError(err_msg(f'Error in dat file on line {i + 1:n}/{len(text):n}, {line.split(" ")[0]:s} keyword found out of context.', text, i, '$', 2))

    if read == 0:
      raise ReadError(f'Dat file missing $ENTER -> $EXIT command blocks.')


#--------------------------------------------------------------------------------------- COMPONENT ---# {{{1
class Component(Variant):
  begin = '$ENTER COMPONENT'
  command = 'COMPONENT'
  end = '$EXIT COMPONENT'
  type = 'COMPONENT'

  @hybridmethod
  def read(cls, text: list, line_start: int = 0):
    if type(text) is str:
      text = text.split('\n')
    elif type(text) is not list:
      raise ValueError('{cls.__name__:s}.read() text must be a list of str or a str, not {type(text).__name__:s}.')

    kompo = None
    description = list()
    first_line = -1
    last_line = -1
    while i < len(text) - 1:
      line = single_spaces(text[i].upper()).strip()
      if line.startswith('!'):
        if line == '!' and len(description) == 0:
          continue
        else:
          description.append(text[i])

      elif line.startswith(cls.begin):
        first_line = i + 1
        name = get_name(text[i])
        doftype = get_doftype(line)
        if '!' in line:
          description.append('! ' + text[i].split('!', 1)[1])
        if len(description) == 0:
          description = None
        kompo = cls(name=name, doftype=doftype, description=description)

      elif line.startswith(cls.end):
        if first_line == -1:
          raise ReadError(err_msg(f'{self.type:s}.read() $EXIT command without $ENTER command on line {line_start + i:n}.', text, i, '$EXIT', 2))
        last_line = i - 1
        break

    if kompo is None:
      raise ReadError(f'{self.type:s}.read() missing $ENTER -> $EXIT command block.')
    else:
      kompo.read(text[first_line:last_line + 1], first_line)

  @read.instancemethod
  def read(self, text:list, line_start: int = 0):
    pass




  def __init__(self, name: str, doftype: list = None, description = None):
    super().__init__(name=name, description=description)
    self.doftype = doftype

#-------------------------------------------------------------------------------------------- MAIN ---# {{{1
if __name__ == '__main__':
  logger = logging.getLogger()
  logger_handler = logger.handlers[0]
  logger_handler.setFormatter(ADVANCED_FORMAT)
  logger_handler.setLevel(logging.DEBUG)
  logging.info(f'{__file__} started')

  m = Model()
  m.read_file('/mnt/c/HONZA/programming/pythonFEA/res/datfiles/beam.dat')

#---------------------------------------------------------------------------------------- MODELINE ---# {{{1
# vim: set foldmethod=marker foldenable:
