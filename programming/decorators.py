from functools import wraps


def debug(func):
    msg = func.__qualname__

    @wraps(func)
    def wrapper(*args, **kwargs):
        print(msg)

        return func(*args, **kwargs)

    return wrapper


def debug_methods(cls):
    '''Only instance methods get wrapped'''

    for name, val in vars(cls).items():
        if callable(val):
            setattr(cls, name, debug(val))

    return cls


def debug_attr(cls):
    '''Override methods in the class'''

    original_getattribute = cls.__getattribute__

    def __getattribute__(self, name):
        print('Get:', name)
        return original_getattribute(self, name)

    cls.__getattribute__ = __getattribute__

    return cls


class DebugClass(type):
    '''Usage: class Foo(metaclass=debug_class): ...'''

    def __new__(cls, clsname, bases, clsdict):
        class_obj = super().__new__(cls, clsname, bases, clsdict)
        class_obj = debug_methods(class_obj)

        return class_obj
