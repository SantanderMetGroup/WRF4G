__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

def get_similar_commands( name, sub_commands ):
    """
    Command name auto-correct.
    """
    from difflib import get_close_matches
    name = name.lower()
    close_commands = get_close_matches( name, sub_commands )
    if close_commands:
        return close_commands[0]
    else:
        return None
