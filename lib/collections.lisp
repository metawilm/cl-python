;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CLPYTHON.MODULE.COLLECTIONS; Readtable: PY-AST-USER-READTABLE -*-
;;
;; This software is Copyright (c) Franz Inc. and Willem Broekema.
;; Franz Inc. and Willem Broekema grant you the rights to
;; distribute and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :clpython.module.collections)
(clpython::in-syntax clpython::*ast-user-readtable*)

(defun |namedtuple| (&rest args)
  (declare (optimize (debug 3) (safety 3)))
  (let* ((globals-ht (clpython::make-eq-hash-table))
         (clpython::*habitat* (clpython::make-habitat))
         (mod-namespace (clpython::make-hash-table-ns
                         :dict-form 'globals-ht
                         :scope :module
                         :parent (clpython::make-builtins-namespace)))
         (clpython::*module-namespace* mod-namespace))
    (clpython::with-module-toplevel-context ()
      ;; Implemented as described in http://code.activestate.com/recipes/500261/
      ([exec-stmt] "
from operator import itemgetter as _itemgetter
from keyword import iskeyword as _iskeyword
import sys as _sys

def f(typename, field_names, verbose=False, rename=False):
    # Parse and validate the field names.  Validation serves two purposes,
    # generating informative error messages and preventing template injection attacks.
    if isinstance(field_names, basestring):
        field_names = field_names.replace(',', ' ').split() # names separated by whitespace and/or commas
    field_names = tuple(map(str, field_names))
    if rename:
        names = list(field_names)
        seen = set()
        for i, name in enumerate(names):
            if (not min(c.isalnum() or c=='_' for c in name) or _iskeyword(name)
                or not name or name[0].isdigit() or name.startswith('_')
                or name in seen):
                    names[i] = '_%d' % i
            seen.add(name)
        field_names = tuple(names)
    for name in (typename,) + field_names:
        if not min(c.isalnum() or c=='_' for c in name):
            raise ValueError('Type names and field names can only contain alphanumeric characters and underscores: %r' % name)
        if _iskeyword(name):
            raise ValueError('Type names and field names cannot be a keyword: %r' % name)
        if name[0].isdigit():
            raise ValueError('Type names and field names cannot start with a number: %r' % name)
    seen_names = set()
    for name in field_names:
        if name.startswith('_') and not rename:
            raise ValueError('Field names cannot start with an underscore: %r' % name)
        if name in seen_names:
            raise ValueError('Encountered duplicate field name: %r' % name)
        seen_names.add(name)
     # Create and fill-in the class template
    numfields = len(field_names)
    argtxt = repr(field_names).replace('\\'', '')[1:-1]   # tuple repr without parens or quotes
    reprtxt = ', '.join('%s=%%r' % name for name in field_names)
    template = '''class %(typename)s(tuple):
        '%(typename)s(%(argtxt)s)' \\n
        __slots__ = () \\n
        _fields = %(field_names)r \\n
        def __new__(_cls, %(argtxt)s):
            return _tuple.__new__(_cls, (%(argtxt)s)) \\n
        @classmethod
        def _make(cls, iterable, new=tuple.__new__, len=len):
            'Make a new %(typename)s object from a sequence or iterable'
            result = new(cls, iterable)
            if len(result) != %(numfields)d:
                raise TypeError('Expected %(numfields)d arguments, got %%d' %% len(result))
            return result \\n
        def __repr__(self):
            return '%(typename)s(%(reprtxt)s)' %% self \\n
        def _asdict(self):
            'Return a new dict which maps field names to their values'
            return dict(zip(self._fields, self)) \\n
        def _replace(_self, **kwds):
            'Return a new %(typename)s object replacing specified fields with new values'
            result = _self._make(map(kwds.pop, %(field_names)r, _self))
            if kwds:
                raise ValueError('Got unexpected field names: %%r' %% kwds.keys())
            return result \\n
        def __getnewargs__(self):
            return tuple(self) \\n\\n''' % locals()
    for i, name in enumerate(field_names):
        template += '        %s = _property(_itemgetter(%d))\\n' % (name, i)
    if verbose:
        print template
     # Execute the template string in a temporary namespace
    namespace = dict(_itemgetter=_itemgetter, __name__='namedtuple_%s' % typename,
                     _property=property, _tuple=tuple)
    try:
        exec template in namespace
    except SyntaxError, e:
        raise SyntaxError(e.message + ':\\n' + template)
    result = namespace[typename]

    # For pickling to work, the __module__ variable needs to be set to the frame
    # where the named tuple is created.  Bypass this step in enviroments where
    # sys._getframe is not defined (Jython for example) or sys._getframe is not
    # defined for arguments greater than 0 (IronPython).

    # CLPython TODO: disabled this for now
    #try:
    #    result.__module__ = _sys._getframe(1).f_globals.get('__name__', '__main__')
    #except (AttributeError, ValueError):
    #    pass
    return result" nil nil))
    (apply (gethash '{f} globals-ht) args)))