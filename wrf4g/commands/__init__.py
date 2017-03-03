#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

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
