local awful = require('awful')
local beautiful = require('beautiful')
local hotkeys_popup = require("awful.hotkeys_popup")
local wibox = require('wibox')
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

local programs = require('ohmree.programs')

local widgets = {}

widgets.awesomemenu = {
  { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
  { "manual", programs.terminal .. " -e man awesome" },
  { "edit config", programs.editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  { "quit", function() awesome.quit() end },
}

widgets.mainmenu = awful.menu({ items = { { "awesome", widgets.awesomemenu, beautiful.awesome_icon },
							{ "open terminal", programs.terminal }
}
					   })

widgets.launcher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = widgets.mainmenu })

-- }}}

-- Keyboard map indicator and switcher
widgets.keyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
widgets.textclock = wibox.widget.textclock()

return widgets
