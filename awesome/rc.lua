-- {{{ License --
-- Awesome configuration, using awesome 3.4.10 on Arch GNU/Linux
--   * Adrian C. <anrxc@sysphere.org>

-- Screenshot: http://sysphere.org/gallery/snapshots

-- This work is licensed under the Creative Commons Attribution-Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
-- }}}


-- {{{ Libraries
local awful = require("awful")
awful.rules = require("awful.rules")
awful.autofocus = require("awful.autofocus")
awful.widget = require("awful.widget")
-- User libraries
local wibox = require("wibox")
local vicious = require("vicious")
local scratch = require("scratch")
local beautiful = require("beautiful")
local debian = {}
debian.menu = require("debian.menu")
-- notifications:
local naughty = require("naughty")
-- }}}

local keydoc = require("keydoc")

if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
local altkey = "Mod1"
local winkey = "Mod4"
-- local modkey = "Mod4"
local modkey = "Mod1"

local home   = os.getenv("HOME")
local exec   = awful.util.spawn
local sexec  = awful.util.spawn_with_shell
local scount = screen.count()

--local hostname = awful.util.pread('hostname -s'):gsub('\n', '')
--if awful.util.file_readable(host_config_file) then
        --local host_config_function, host_config_load_error
        --host_config_function, host_config_load_error = loadfile(host_config_file)
        --if not host_config_load_error then
                --host_config_function()
        --else
                --print(string.format('[awesome] Failed to load %s: %s', host_config_file, host_config_load_error))
        --end
--end

--awful.util.spawn("xscreensaver -nosplash")
--awful.util.spawn_with_shell("if [ -z `pidof nm-applet` ]; then nm-applet; fi")
--awful.util.spawn_with_shell("if [ -z `pidof compton` ]; then compton; fi")
--awful.util.spawn_with_shell("if [ -z `pidof conky` ]; then conky -c ~/dotfiles/conky/conkyrc; fi")
--awful.util.spawn_with_shell("if [ -z `pidof conky` ]; then conky -c ~/dotfiles/conky/calendar.conkyrc; fi")
--awful.util.spawn_with_shell("if [ -z `pidof bluetooth-applet` ]; then bluetooth-applet; fi")
--awful.util.spawn_with_shell("xsetkbmap us")
--awful.util.spawn_with_shell("sleep 1 && xmodmap /home/dbu/.xmodmap")

-- notifications:
-- naughty.config.default_preset.timeout = 5
-- naughty.config.default_preset.position = "bottom_right"
-- naughty.config.default_preset.screen           = 1
-- naughty.config.default_preset.font = 'Ubuntu Mono 14'

-- Beautiful theme
beautiful.init(home .. "/.config/awesome/zenburn.lua")

--zenburn.lua

-- Window management layouts
default_layout = awful.layout.suit.title

layouts = {
  awful.layout.suit.tile.right,     -- 1
  awful.layout.suit.floating,       -- 2
  awful.layout.suit.fair            -- 3
-- awful.layout.suit.tile.bottom,    -- 4
-- awful.layout.suit.magnifier,      -- 5
-- awful.layout.suit.max,            -- 6
}
-- }}}


-- {{{ Tags
tags = {
   names  = {"terminal",        "emacs",        "www",        "slack",        "spotify",  "misc"},
  --         1           2           3           4           5           6           7           8           9
   layout = { layouts[1], layouts[1], layouts[1], layouts[3], layouts[2], layouts[2], layouts[1]}
}

for s = 1, scount do
  tags[s] = awful.tag(tags.names, s, tags.layout)
  for i, t in ipairs(tags[s]) do
      awful.tag.setproperty(t, "mwfact", i==5 and 0.13  or  0.5)
      awful.tag.setproperty(t, "hide",  (i==9) and true)
  end
end
-- }}}


function run_once(prg,arg_string,pname,screen)
    if not prg then
        do return nil end
    end

    if not pname then
       pname = prg
    end

    if not arg_string then
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. ")",screen)
    else
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. " " .. arg_string .. ")",screen)
    end
end

-- run_once("/usr/bin/pidgin",nil,nil,1)

terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor
-- {{{ Wibox
--
-- {{{ Widgets configuration
-- //////////////////////////////////////////////////////////////////////////////


-- Volume widget
volumecfg = {}
volumecfg.cardid  = 0
volumecfg._volume  = 0
volumecfg._notify = nil
volumecfg.channel = "Master"
volumecfg.widget = wibox.widget.textbox("")
volumecfg.widget.align = "right"

volumecfg_t = awful.tooltip({ objects = { volumecfg.widget },})
volumecfg_t:set_text("Volume")

local alsawidget =
{
	channel = "Master",
	step = "5%",
	colors =
	{
		unmute = "#AECF96",
		mute = "#FF5656"
	},
	mixer = terminal .. " -e alsamixer", -- or whatever your preferred sound mixer is
	notifications =
	{
		icons =
		{
			-- the first item is the 'muted' icon
			"/usr/share/icons/ubuntu-mono-light/status/24/audio-volume-muted.svg",
			-- the rest of the items correspond to intermediate volume levels - you can have as many as you want (but must be >= 1)
			"/usr/share/icons/ubuntu-mono-light/status/24/audio-volume-low-panel.svg",
			"/usr/share/icons/ubuntu-mono-light/status/24/audio-volume-high-panel.svg"
		},
		font = "Monospace 11", -- must be a monospace font for the bar to be sized consistently
		icon_size = 48,
		bar_size = 40 -- adjust to fit your font if the bar doesn't fit
	}
}

-- command must start with a space!
volumecfg.mixercommand = function (command)
       local fd = io.popen("amixer -c " .. volumecfg.cardid .. command)
       local status = fd:read("*all")
       fd:close()

       local volume = string.match(status, "(%d?%d?%d)%%")
       alsawidget._current_level = tonumber(volume)
       volume = string.format("% 3d", volume)
       status = string.match(status, "%[(o[^%]]*)%]")
       if string.find(status, "on", 1, true) then
               volume = volume .. "%"
               alsawidget._muted = nil
       else
               alsawidget._muted = t
               volume = volume .. "M"
       end
       volumecfg.widget.text = volume
       volumecfg.notify()
end

volumecfg.notify = function ()
	local preset =
	{
		height = 30,
		width = 150,
		font = "Monospace 11",
        position = "bottom_right",
        screen = 1
	}
	local i = 1;
	while alsawidget.notifications.icons[i + 1] ~= nil
	do
		i = i + 1
	end
	if i >= 2
	then
		preset.icon_size = alsawidget.notifications.icon_size
		if alsawidget._muted or alsawidget._current_level == 0
		then
			preset.icon = alsawidget.notifications.icons[1]
		elseif alsawidget._current_level == 100
		then
			preset.icon = alsawidget.notifications.icons[i]
		else
			local int = math.modf (alsawidget._current_level / 100 * (i - 1))
			preset.icon = alsawidget.notifications.icons[int + 2]
		end
	end
	if alsawidget._muted
	then
		preset.title = alsawidget.channel .. " - Muted"
	elseif alsawidget._current_level == 0
	then
		preset.title = alsawidget.channel .. " - 0% (muted)"
		preset.text = "[" .. string.rep (" ", alsawidget.notifications.bar_size) .. "]"
	elseif alsawidget._current_level == 100
	then
		preset.title = alsawidget.channel .. " - 100% (max)"
		preset.text = "[" .. string.rep ("|", alsawidget.notifications.bar_size) .. "]"
	else
		local int = math.modf (alsawidget._current_level / 100 * alsawidget.notifications.bar_size)
		preset.title = alsawidget.channel .. " - " .. alsawidget._current_level .. "%"
		preset.text = "[" .. string.rep ("|", int) .. string.rep (" ", alsawidget.notifications.bar_size - int) .. "]"
	end
   if volumecfg._notify ~= nil
	then
		
		volumecfg._notify = naughty.notify (
		{
            icon = preset.icon,
			replaces_id = volumecfg._notify.id,
			preset = preset,
            timeout = 1
		})
	else
		volumecfg._notify = naughty.notify ({ preset = preset, timeout = 1 })
	end   
end

volumecfg.update = function ()
       volumecfg.mixercommand(" sget " .. volumecfg.channel)
end
volumecfg.up = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 1%+")
end
volumecfg.down = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " 1%-")
end
volumecfg.toggle = function ()
       volumecfg.mixercommand(" sset " .. volumecfg.channel .. " toggle")
       awful.util.spawn("amixer sset " .. "Speaker" .. " unmute")
       awful.util.spawn("amixer sset " .. "Headphone" .. " unmute")
       awful.util.spawn("amixer sset " .. "Front" .. " unmute")
       awful.util.spawn("amixer sset " .. "Center" .. " unmute")
       awful.util.spawn("amixer sset " .. "Surround" .. " unmute")
       awful.util.spawn("amixer sset " .. "LFE" .. " unmute")
end
volumecfg.widget:buttons(awful.util.table.join(
        awful.button({ }, 4, function () volumecfg.up() end),
        awful.button({ }, 5, function () volumecfg.down() end),
        awful.button({ }, 1, function () volumecfg.toggle() end)
))
volumecfg.update()



mymainmenu = awful.menu({ items = { { "Quit", awesome.quit },
                                    { "Restart", awesome.restart },
                                    { "Logout", function ()  awful.util.spawn("gnome-session-quit") end },
                                    { "Suspend", "dbus-send --system --type=method_call --dest=org.freedesktop.UPower --print-reply /org/freedesktop/UPower org.freedesktop.UPower.Suspend" },
                                    { "Edit config", editor_cmd .. " " .. awesome.conffile },
                                    { "Spotify", 'spotify'},
                                    { "Network", 'nm-applet'},
                                    { "Screenshot", function() sexec("sleep 1 && scrot -s /home/dbu/Downloads/capture-%Y-%m-%d_$wx$h.png") end },
                                    { "Debian", debian.menu.Debian_menu.Debian },
                                  }
                        })
mylauncher = awful.widget.launcher({ image = beautiful.widget_date,
                                     menu = mymainmenu })

-- //////////////////////////////////////////////////////////////////////////////
--
-- {{{ Reusable separator
separator = wibox.widget.imagebox(beautiful.widget_sep)
-- }}}

-- {{{ CPU usage and temperature
cpuicon = wibox.widget.imagebox(beautiful.widget_cpu)
-- Initialize widgets
cpugraph  = awful.widget.graph()
tzswidget = wibox.widget.textbox("")
-- Graph properties
cpugraph:set_width(40):set_height(40)
cpugraph:set_background_color(beautiful.fg_off_widget)
-- cpugraph:set_gradient_angle(0):set_gradient_colors({
--   beautiful.fg_end_widget, beautiful.fg_center_widget, beautiful.fg_widget})
-- Register widgets
vicious.register(cpugraph,  vicious.widgets.cpu,      "$1")
vicious.register(tzswidget, vicious.widgets.thermal, " $1C", 19, "thermal_zone0")
-- }}}

-- {{{ Network state
txwidget = wibox.widget.textbox("")
rxwidget = wibox.widget.textbox("")
vicious.register(txwidget, vicious.widgets.net,
                 "tx: ${enp0s31f6 up_kb}KB", 2)
vicious.register(rxwidget, vicious.widgets.net,
                 "rx: ${enp0s31f6 down_kb}KB", 2)

-- {{{ File system usage
fsicon = wibox.widget.imagebox(beautiful.widget_fs)
-- Initialize widgets
fs = {
  b = awful.widget.progressbar(), r = awful.widget.progressbar(),
  h = awful.widget.progressbar(), s = awful.widget.progressbar()
}
-- Progressbar properties
for _, w in pairs(fs) do
  w:set_vertical(true):set_ticks(true)
  w:set_height(40):set_width(5):set_ticks_size(2)
  w:set_border_color(beautiful.border_widget)
  w:set_background_color(beautiful.fg_off_widget)
--  w:set_gradient_colors({ beautiful.fg_widget,
--     beautiful.fg_center_widget, beautiful.fg_end_widget
--  }) -- Register buttons

end -- Enable caching
vicious.cache(vicious.widgets.fs)
-- Register widgets
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}",     599)
-- }}}

-- {{{ Memory usage
memicon = wibox.widget.imagebox(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_height(40):set_width(8):set_ticks_size(2)
membar:set_background_color(beautiful.fg_off_widget)
--membar:set_gradient_colors({ beautiful.fg_widget,
--   beautiful.fg_center_widget, beautiful.fg_end_widget
--}) -- Register widget
vicious.register(membar, vicious.widgets.mem, "$1", 13)
-- }}}


-- {{{ Volume level
volicon = wibox.widget.imagebox(beautiful.widget_vol)
-- Initialize widgets
volwidget = wibox.widget.textbox("")
-- Progressbar properties
vicious.cache(vicious.widgets.volume)
-- Register widgets

-- {{{ Date and time
-- Initialize widget
datewidget = wibox.widget.textbox("")
-- Register widget
vicious.register(datewidget, vicious.widgets.date, " %e/%m, %R", 61)
-- Register buttons
-- }}}

-- {{{ System tray
systray = wibox.widget.systray()
-- }}}
-- }}}

-- {{{ Wibox initialisation
mywibox     = {}
promptbox = {}
layoutbox = {}
taglist   = {}
taglist.buttons = awful.util.table.join(
    awful.button({ },        1, awful.tag.viewonly),
    awful.button({ modkey }, 1, awful.client.movetotag),
    awful.button({ },        3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, awful.client.toggletag),
    awful.button({ },        4, awful.tag.viewnext),
    awful.button({ },        5, awful.tag.viewprev
))

-- Create an ACPI widget

for s = 1, scount do
    -- Create a promptbox
    promptbox[s] = awful.widget.prompt()
    -- Create a layoutbox
    layoutbox[s] = awful.widget.layoutbox(s)
    layoutbox[s]:buttons(awful.util.table.join(
        awful.button({ }, 1, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        awful.button({ }, 4, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
    ))

    -- Create the taglist
    taglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist.buttons)
    -- Create the wibox
    mywibox[s] = awful.wibox({      screen = s,
        fg = beautiful.fg_normal, height = 40,
        bg = beautiful.bg_normal, position = "bottom",
        border_color = beautiful.fg_normal,
        border_width = 0,
        opacity = 0.8
    })
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(taglist[s])
    left_layout:add(layoutbox[s])
    left_layout:add(separator)
    left_layout:add(promptbox[s])


    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(systray) end
    right_layout:add(separator)
    right_layout:add(fs.r)
    right_layout:add(fsicon)
    right_layout:add(separator)
    right_layout:add(membar)
    right_layout:add(memicon)
    right_layout:add(separator)
    right_layout:add(volumecfg.widget)
    right_layout:add(volwidget)
    right_layout:add(volicon)
    right_layout:add(separator)
    right_layout:add(tzswidget)
    right_layout:add(separator)
    right_layout:add(cpugraph)
    right_layout:add(cpuicon)
    right_layout:add(separator)
    right_layout:add(txwidget)
    right_layout:add(separator)
    right_layout:add(rxwidget)
    right_layout:add(separator)
    right_layout:add(mylauncher)
    right_layout:add(datewidget)

    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_right(right_layout)
    mywibox[s]:set_widget(layout)
end
-- }}}
-- }}}


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 1, function () mymainmenu:hide() end),
    awful.button({ }, 3, function () mymainmenu:toggle() end)
))

-- Client bindings
clientbuttons = awful.util.table.join(
    awful.button({ },        1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ winkey }, 1, awful.mouse.client.resize),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}


-- {{{ Key bindings
--
-- {{{ Global keys
globalkeys = awful.util.table.join(
    -- {{{ Applications
    keydoc.group("Volume"),
    awful.key({ modkey, "Shift",  }, "F2",    function ()
          awful.prompt.run({ prompt = "Rename tab: ", text = awful.tag.selected().name, },
             promptbox[mouse.screen].widget,
             function (s)
                awful.tag.selected().name = s
          end)
    end, "Rename tag"),
    awful.key({                   }, "XF86AudioMute",         function () volumecfg.toggle()                end, "Mute Volume"),
    awful.key({                   }, "XF86AudioLowerVolume",  function () volumecfg.down()                  end, "Lower Volume"),
    awful.key({                   }, "XF86AudioRaiseVolume",  function () volumecfg.up()                    end, "Raise Volume"),
-- Run or raise applications with dmenu
    keydoc.group("Launch applications"),
    awful.key({ modkey, "Shift" }, "p",                     function () awful.util.spawn( "dmenu_run -b -p 'Run command:'" )   end, "Launch application"),
    awful.key({ modkey            }, "t",                     function () awful.util.spawn(terminal)   end, "Spawn terminal"),
    awful.key({ winkey }, "g", function ()
        awful.prompt.run({ prompt = "Web: " }, promptbox[mouse.screen].widget,
            function (command)
                sexec("google-chrome-stable 'http://google.com/search?q="..command.."'")
--                awful.tag.viewonly(tags[scount][8])
            end)
    end, "Search google"),
    keydoc.group("Awesome commands"),
--    awful.key({ modkey            }, "Escape",                awful.tag.history.restore, "Restore window history"),
    awful.key({ modkey, "Control" }, "r",                     awesome.restart, "Restart awesome"),
    awful.key({ modkey, "Control" }, "l",                     function () awful.util.spawn("gnome-screensaver-command -l") end, "Lock screen"),
    awful.key({                   }, "XF86Launch1",          function () awful.util.spawn("gnome-screensaver-command -l") end, "Lock Screen"),
    awful.key({ modkey            }, "o",                     awful.client.movetoscreen, "Move window to next screen"),
    awful.key({ modkey, "Shift" }, "F1", keydoc.display),
    awful.key({ modkey, "Shift" }, "XF86AudioPlay", keydoc.display),
    awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative(-1) end, "Move focus to next monitor"),



    -- {{{ Awesome controls
    awful.key({ modkey, "Shift" }, "q", awesome.quit, "Quit awesome"),
    awful.key({ modkey, "Shift" }, "r", function ()
        promptbox[mouse.screen].text = awful.util.escape(awful.util.restart())
    end, "Restart Awesome"),
    -- }}}


    awful.key({ winkey }, "x",
    function ()
      awful.prompt.run({ prompt = "Run Lua code: " },
      promptbox[mouse.screen].widget,
      awful.util.eval, nil,
      awful.util.getdir("cache") .. "/history_eval")
    end, "Eval Lua Code"),
    -- {{{ Tag browsing
    awful.key({ altkey, "Control" }, "n",   awful.tag.viewnext, "Move to next tag"),
    awful.key({ altkey, "Control" }, "p",   awful.tag.viewprev, "Move to previous tag"),
    awful.key({ altkey, "Control" }, "Right",   awful.tag.viewnext, "Move to next tag"),
    awful.key({ altkey, "Control" }, "Left",   awful.tag.viewprev, "Move to previous tag"),
    awful.key({ altkey }, "Escape", function ()
        awful.menu.menu_keys.down = { "Down", "Alt_L" }
        local cmenu = awful.menu.clients({width=230}, { keygrabber=true, coords={x=525, y=330} })
    end, "List windows")
)
    -- }}}

    -- {{{ Layout manipulation
clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift" }, "l", function () awful.tag.incmwfact(0.05) end),
    awful.key({ modkey, "Shift" }, "h", function () awful.tag.incmwfact(-0.05) end),
--    awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end, "Change window layout backwards"),
    awful.key({ modkey },          "space", function () awful.layout.inc(layouts,  1) end, "Change window layout"),
    -- }}}

    -- switch between monitors:
--    awful.key({ modkey, "Control" }, "l", function () awful.screen.focus_relative( 1) end),

    awful.key({ modkey }, "s", function () scratch.pad.toggle() end, "Toggle scratchpad"),
--    awful.key({ modkey }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey }, "F12", function () scratch.drop("x-terminal-emulator", "bottom") end, "Console at bottom"),

    awful.key({ modkey }, "j", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end, "Tab between windows"),
    awful.key({ modkey }, "Tab", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end, "Tab between windows"),
    awful.key({ modkey }, "k", function ()
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end, "Tab backwards between windows"),
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1)  end, "Swap window right"),
    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end, "Swap window left"),
    -- }}}
    keydoc.group("Window manipulation"),
    awful.key({ modkey              }, "c", function (c) c:kill() end, "Kill window"),
--    awful.key({ modkey              }, "d", function (c) scratch.pad.set(c, 0.60, 0.60, true) end),
    awful.key({ winkey              }, "f", function (c) c.fullscreen = not c.fullscreen end, "Fullscreen"),
    awful.key({ modkey, "Control"     }, "m", function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
    end, "Maximize window"),
    awful.key({ modkey              }, "o",     awful.client.movetoscreen, "Move window to next screen"),
    awful.key({ winkey              }, "Next",  function () awful.client.moveresize( 20,  20, -40, -40) end),
    awful.key({ winkey              }, "Prior", function () awful.client.moveresize(-20, -20,  40,  40) end),
    awful.key({ winkey              }, "Down",  function () awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ winkey              }, "Up",    function () awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ winkey              }, "Left",  function () awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ winkey              }, "Right", function () awful.client.moveresize( 20,   0,   0,   0) end),
    awful.key({ modkey, "Control", "Shift"   }, "Left",
       function (c)
          local curidx = awful.tag.getidx(c:tags()[1])
          if curidx == 1 then
             c:tags({screen[mouse.screen]:tags()[6]})
          else
             c:tags({screen[mouse.screen]:tags()[curidx - 1]})
          end
          awful.tag.viewprev()
    end, "Move focused window to next tag"),
    awful.key({ modkey, "Control", "Shift"   }, "Right",
       function (c)
          local curidx = awful.tag.getidx(c:tags()[1])
          if curidx == 6 then
             c:tags({screen[mouse.screen]:tags()[1]})
          else
             c:tags({screen[mouse.screen]:tags()[curidx + 1]})
          end
          awful.tag.viewnext()
    end, "Move focused window to previous tag"),
    -- awful.key({ modkey, "Control", "Shift" }, "Right", function ()
    --       local newtag = tonumber(awful.tag.selected(client.focus.screen).name) + 1
    --       if newtag == 9 then
    --          newtag = 1
    --       end
    --       awful.client.movetotag(tags[client.focus.screen][newtag])
    --       awful.tag.viewnext()
    -- end, "Move focused window to next tag"),
    -- awful.key({ modkey, "Control", "Shift" }, "Left", function ()
    --       local newtag = tonumber(awful.tag.selected(client.focus.screen).name) - 1
    --       if newtag == 0 then
    --          newtag = 8
    --       end
    --       awful.client.movetotag(tags[client.focus.screen][newtag])
    --       awful.tag.viewprev()
    -- end, "Move focused window to previous tag"),
    awful.key({ modkey, "Shift"     }, "t", function (c)
        if   c.titlebar then awful.titlebar.remove(c)
        else awful.titlebar.add(c, { modkey = modkey }) end
    end, "Toggle titlebars")
)
-- }}}

-- {{{ Keyboard digits
local keynumber = 0
for s = 1, scount do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end
-- }}}

-- {{{ Tag controls
for i = 1, keynumber do
    globalkeys = awful.util.table.join( globalkeys,
--        awful.key({ modkey }, "#" .. i + 9, function ()
--            local screen = mouse.screen
--            if tags[screen][i] then awful.tag.viewonly(tags[screen][i]) end
--        end),
        awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
            local screen = mouse.screen
            if tags[screen][i] then awful.tag.viewtoggle(tags[screen][i]) end
        end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
            if client.focus and tags[client.focus.screen][i] then
                awful.client.movetotag(tags[client.focus.screen][i])
            end
        end)
        )
end
-- }}}

-- Set keys
root.keys(globalkeys)
-- }}}


-- {{{ Rules
awful.rules.rules = {
    { rule = { }, properties = {
      focus = true,      size_hints_honor = false,
      keys = clientkeys, buttons = clientbuttons,
      border_width = beautiful.border_width,
      border_color = beautiful.border_normal }
    },

-- all tags: ---------------------------------------------------------------------------------------
    { rule = { class = "Gimp", instance="_Remember_" },
      properties = { floating = true } },

    { rule = { class = "Evince" },
      properties = { floating = true, instance="_Remember_" } },

    { rule = { class = "Deluge" },
      properties = { floating = true, instance="_Remember_" } },

   { rule = { class = "Vlc" },
      properties = { floating = true, maximized_horizontal = true, maximized_vertical = true } },

   { rule = { class = "Pexip Infinity Connect" },
      properties = { floating = true, instance="_Remember_" } },

-- tag 1:(code) -----------------------------------------------------------------------------------
--    { rule = { class = "Emacs", instance = "emacs" },
--      properties = { maximized_vertical = true, width = 1700, size_hints_honor = true, tag = tags[scount][1], floating = false } },
--

-- tag 2:(vim) ------------------------------------------------------------------------------------
    -- { rule = { class = "Gvim" },
    --properties = { size_hints_honor = true, tag = tags[scount][2] } },

-- tag 3:(sh01) ----------------------------------------------------------------------------------------

-- tag 4:(sh02) -----------------------------------------------------------------------------------

-- tag 5:(im) -------------------------------------------------------------------------------------
    {  rule = { class = "Pidgin", instance="_Remember_" },
      properties = { floating = true, width = 800, tag = tags[scount][5] } },

    { rule = { instance = "empathy-chat" },
      properties = { floating = true, width = 800, tag = tags[scount][5] } },
    { rule = { instance = "empathy-auth-client" },
      properties = { floating = true, tag = tags[scount][5] } },
    { rule = { class = "Empathy" },
      properties = { floating = true, maximized_vertical = true, tag = tags[scount][5] } },

-- tag 6:(vm) -------------------------------------------------------------------------------------
    { rule = { class = "Vmware" },
      properties = { floating = true, tag = tags[scount][6], instance="_Remember_" } },

-- tag 7:(mail) -----------------------------------------------------------------------------------

-- tag 8:(www) ------------------------------------------------------------------------------------

}
-- }}}


-- {{{ Signals
--
-- {{{ Manage signal handler
client.connect_signal("manage", function (c, startup)
    -- Add titlebar to floaters, but remove those from rule callback
    -- if awful.client.floating.get(c)
    -- or awful.layout.get(c.screen) == awful.layout.suit.floating then
    --     if   c.titlebar then awful.titlebar.remove(c)
    --     else awful.titlebar.add(c, {modkey = modkey}) end
    -- end

    -- -- Enable sloppy focus (focus follows mouse)
     c:connect_signal("mouse::enter", function (c)
         if  awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
         and awful.client.focus.filter(c) then
             client.focus = c
         end
     end)

    -- Client placement
    if not startup then
        awful.client.setslave(c)

        if  not c.size_hints.program_position
        and not c.size_hints.user_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end
end)
-- }}}

-- {{{ Focus signal handlers
client.connect_signal("focus",   function (c)
  c.border_color = beautiful.border_focus
  c.opacity = 1
end)
client.connect_signal("unfocus", function (c)
  c.border_color = beautiful.border_normal
  c.opacity = 0.8
end)
-- }}}

-- {{{ Arrange signal handler
for s = 1, scount do screen[s]:connect_signal("arrange", function ()
    local clients = awful.client.visible(s)
    local layout = awful.layout.getname(awful.layout.get(s))

    for _, c in pairs(clients) do -- Floaters are always on top
        if   awful.client.floating.get(c) or layout == "floating"
        then if not c.fullscreen then c.above       =  true  end
        else                          c.above       =  false end
    end
  end)
end
-- }}}
-- }}}
