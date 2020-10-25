local menubar = require('menubar')
programs = {}

programs.terminal = 'kitty'
programs.editor = os.getenv('EDITOR') or terminal .. '-e vi'
programs.editor_cmd = 'emacsclient -nqc -a=""'

-- Menubar configuration
menubar.utils.terminal = programs.terminal -- Set the terminal for applications that require it

return programs
