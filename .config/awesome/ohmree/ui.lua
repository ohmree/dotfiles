local awful = require('awful')
local gears = require('gears')
local beautiful = require('beautiful')
local wibox = require('wibox')
local naughty = require('naughty')

local keybinds = require('ohmree.keybinds')
local widgets = require('ohmree.widgets')

local ui = {}

ui.set_wallpaper = function(s)
  -- Wallpaper
  if beautiful.wallpaper then
	local wallpaper = beautiful.wallpaper
	-- If wallpaper is a function, call it with the screen
	if type(wallpaper) == "function" then
	  wallpaper = wallpaper(s)
	end
	gears.wallpaper.maximized(wallpaper, s, true)
  end
end

ui.setup_screen = function(s)
    -- Wallpaper
    ui.set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = keybinds.taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = keybinds.tasklist_buttons
    }

    -- Create wiboxes
    s.mystatusbar = awful.wibar({ position = "top", screen = s })
	s.mytaskbar = awful.wibar { position = 'bottom', screen = s }
    -- Add widgets to wiboxes
    s.mystatusbar:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
			widgets.launcher,
            s.mytaglist,
            s.mypromptbox,
        },
		-- Middle spacer
		{ layout = wibox.layout.fixed.horizontal },
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            widgets.keyboardlayout,
            wibox.widget.systray(),
            widgets.textclock,
            s.mylayoutbox,
        },
    }

	s.mytaskbar:setup {
		layout = wibox.layout.align.horizontal,
		s.mytasklist,
	}
end

return ui
