-- {{{ License
--
-- Awesome configuration, using awesome 3.4.10 on Arch GNU/Linux
--   * Adrian C. <anrxc@sysphere.org>

-- Screenshot: http://sysphere.org/gallery/snapshots

-- This work is licensed under the Creative Commons Attribution-Share
-- Alike License: http://creativecommons.org/licenses/by-sa/3.0/
-- }}}


-- {{{ Libraries
require("awful")
require("awful.rules")
require("awful.autofocus")
-- User libraries
require("vicious")
require("scratch")
-- notifications:
require("naughty")
-- }}}

if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.add_signal("debug::error", function (err)
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

local hostname = awful.util.pread('hostname -s'):gsub('\n', '')
local host_config_file = awful.util.getdir('config') .. '/rc.' .. hostname .. '.lua'
if awful.util.file_readable(host_config_file) then
        local host_config_function, host_config_load_error
        host_config_function, host_config_load_error = loadfile(host_config_file)
        if not host_config_load_error then
                host_config_function()
        else
                print(string.format('[awesome] Failed to load %s: %s', host_config_file, host_config_load_error))
        end
end

-- awful.util.spawn("xscreensaver -nosplash")
-- awful.util.spawn_with_shell("if [ -z `pidof nm-applet` ]; then nm-applet; fi")
-- awful.util.spawn_with_shell("if [ -z `pidof bluetooth-applet` ]; then bluetooth-applet; fi")
-- awful.util.spawn_with_shell("xsetkbmap us")
-- awful.util.spawn_with_shell("sleep 1 && xmodmap /home/dbu/.xmodmap")

-- notifications:
naughty.config.default_preset.timeout = 5
naughty.config.default_preset.position = "bottom_right"
naughty.config.default_preset.screen           = 1
naughty.config.default_preset.font = 'Ubuntu Mono 10' 

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
  names  = {"1",        "2",        "3",        "4",        "5",        "6",        "7",        "8",        "9" },
  --         1           2           3           4           5           6           7           8           9
  layout = { layouts[1], layouts[1], layouts[1], layouts[3], layouts[2], layouts[2], layouts[2], layouts[2], layouts[2]
}}

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

-- {{{ Wibox
--
-- {{{ Widgets configuration
-- //////////////////////////////////////////////////////////////////////////////
kbdcfg = {}
kbdcfg.cmd = "setxkbmap"
kbdcfg.layout = { "us", "no"}
kbdcfg.current = 1  -- us is our default layout
kbdcfg.widget = widget({ type = "textbox", align = "right" })
kbdcfg.widget.text = "Keyboard: " .. kbdcfg.layout[kbdcfg.current] .. " "
kbdcfg.switch = function ()
   kbdcfg.current = kbdcfg.current % #(kbdcfg.layout) + 1
   local t = "Keyboard: " .. kbdcfg.layout[kbdcfg.current] .. " "
   kbdcfg.widget.text = t
   os.execute(kbdcfg.cmd .. " " .. kbdcfg.layout[kbdcfg.current])
   os.execute("xmodmap ~/.xmodmap")
end

-- Mouse bindings
kbdcfg.widget:buttons(awful.util.table.join(
    awful.button({ }, 1, function () kbdcfg.switch() end)
))


-- Volume widget
volumecfg = {}
volumecfg.cardid  = 0
volumecfg.channel = "Master"
volumecfg.widget = widget({ type = "textbox", name = "volumecfg.widget", align = "right" })
 
volumecfg_t = awful.tooltip({ objects = { volumecfg.widget },})
volumecfg_t:set_text("Volume")
 
-- command must start with a space!
volumecfg.mixercommand = function (command)
       local fd = io.popen("amixer -c " .. volumecfg.cardid .. command)
       local status = fd:read("*all")
       fd:close()
 
       local volume = string.match(status, "(%d?%d?%d)%%")
       volume = string.format("% 3d", volume)
       status = string.match(status, "%[(o[^%]]*)%]")
       if string.find(status, "on", 1, true) then
               volume = volume .. "%"
       else
               volume = volume .. "M"
       end
       volumecfg.widget.text = volume
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
end
volumecfg.widget:buttons({
       -- button({ }, 4, function () volumecfg.up() end),
       -- button({ }, 5, function () volumecfg.down() end),
       -- button({ }, 1, function () volumecfg.toggle() end)
})
volumecfg.update()
 
terminal = "x-terminal-emulator"
editor = os.getenv("EDITOR") or "vim"
editor_cmd = terminal .. " -e " .. editor


mymainmenu = awful.menu({ items = { { "Quit", awesome.quit },
                                    { "Restart", awesome.restart },
                                    { "Logout", function ()  awful.util.spawn("gnome-session-quit") end },
                                    { "Suspend", "gksudo pm-suspend" },
                                    { "Edit config", editor_cmd .. " " .. awesome.conffile },
                                    { "Spotify", 'spotify'},
                                    { "Network", 'nm-applet'},
                                  }
                        })
mylauncher = awful.widget.launcher({ image = image(beautiful.widget_date),
                                     menu = mymainmenu })

-- //////////////////////////////////////////////////////////////////////////////
--
-- {{{ Reusable separator
separator = widget({ type = "imagebox" })
separator.image = image(beautiful.widget_sep)
-- }}}

-- {{{ CPU usage and temperature
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(beautiful.widget_cpu)
-- Initialize widgets
cpugraph  = awful.widget.graph()
tzswidget = widget({ type = "textbox" })
-- Graph properties
cpugraph:set_width(40):set_height(20)
cpugraph:set_background_color(beautiful.fg_off_widget)
cpugraph:set_gradient_angle(0):set_gradient_colors({
   beautiful.fg_end_widget, beautiful.fg_center_widget, beautiful.fg_widget
}) -- Register widgets
vicious.register(cpugraph,  vicious.widgets.cpu,      "$1")
vicious.register(tzswidget, vicious.widgets.thermal, " $1C", 19, "thermal_zone0")
-- }}}

-- {{{ Network state
txwidget = widget({ type="textbox" })
rxwidget = widget({ type="textbox" })
vicious.register(txwidget, vicious.widgets.net,
                 "tx: ${wlan0 up_kb}KB", 2)
vicious.register(rxwidget, vicious.widgets.net,
                 "rx: ${wlan0 down_kb}KB", 2)


-- }}}
-- {{{ Battery state
baticon = widget({ type = "imagebox" })
baticon.image = image(beautiful.widget_bat)
-- Initialize widget
batwidget = widget({ type = "textbox" })
-- Register widget
vicious.register(batwidget, vicious.widgets.bat, "$2% [$3]", 61, "BAT0")
-- }}}

-- {{{ File system usage
fsicon = widget({ type = "imagebox" })
fsicon.image = image(beautiful.widget_fs)
-- Initialize widgets
fs = {
  b = awful.widget.progressbar(), r = awful.widget.progressbar(),
  h = awful.widget.progressbar(), s = awful.widget.progressbar()
}
-- Progressbar properties
for _, w in pairs(fs) do
  w:set_vertical(true):set_ticks(true)
  w:set_height(20):set_width(5):set_ticks_size(2)
  w:set_border_color(beautiful.border_widget)
  w:set_background_color(beautiful.fg_off_widget)
  w:set_gradient_colors({ beautiful.fg_widget,
     beautiful.fg_center_widget, beautiful.fg_end_widget
  }) -- Register buttons

end -- Enable caching
vicious.cache(vicious.widgets.fs)
-- Register widgets
vicious.register(fs.r, vicious.widgets.fs, "${/ used_p}",     599)
-- }}}

-- {{{ Memory usage
memicon = widget({ type = "imagebox" })
memicon.image = image(beautiful.widget_mem)
-- Initialize widget
membar = awful.widget.progressbar()
-- Pogressbar properties
membar:set_vertical(true):set_ticks(true)
membar:set_height(20):set_width(8):set_ticks_size(2)
membar:set_background_color(beautiful.fg_off_widget)
membar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Register widget
vicious.register(membar, vicious.widgets.mem, "$1", 13)
-- }}}


-- {{{ Volume level
volicon = widget({ type = "imagebox" })
volicon.image = image(beautiful.widget_vol)
-- Initialize widgets
volbar    = awful.widget.progressbar()
volwidget = widget({ type = "textbox" })
-- Progressbar properties
volbar:set_vertical(true):set_ticks(true)
volbar:set_height(20):set_width(8):set_ticks_size(2)
volbar:set_background_color(beautiful.fg_off_widget)
volbar:set_gradient_colors({ beautiful.fg_widget,
   beautiful.fg_center_widget, beautiful.fg_end_widget
}) -- Enable caching
vicious.cache(vicious.widgets.volume)
-- Register widgets
vicious.register(volbar, vicious.widgets.volume, "$1", 2, "Master")

-- {{{ Date and time
dateicon = widget({ type = "imagebox" })
-- Initialize widget
datewidget = widget({ type = "textbox" })
-- Register widget
vicious.register(datewidget, vicious.widgets.date, " %e/%m, %R", 61)
-- Register buttons
-- }}}

-- {{{ System tray
systray = widget({ type = "systray" })
-- }}}
-- }}}

-- {{{ Wibox initialisation
wibox     = {}
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

for s = 1, scount do
    -- Create a promptbox
    promptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create a layoutbox
    layoutbox[s] = awful.widget.layoutbox(s)
    layoutbox[s]:buttons(awful.util.table.join(
        awful.button({ }, 1, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
        awful.button({ }, 4, function () awful.layout.inc(layouts,  1) end),
        awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)
    ))

    -- Create the taglist
    taglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, taglist.buttons)
    -- Create the wibox
    wibox[s] = awful.wibox({      screen = s,
        fg = beautiful.fg_normal, height = 20,
        bg = beautiful.bg_normal, position = "bottom",
        border_color = beautiful.fg_normal,
        border_width = 0
    })
    -- Add widgets to the wibox
    wibox[s].widgets = {
        mylauncher, datewidget, dateicon,
        {   taglist[s], layoutbox[s], separator, promptbox[s],
            ["layout"] = awful.widget.layout.horizontal.leftright
        },
        s == 1 and systray or nil,
        -- separator, orgwidget,  orgicon,
        -- separator, mailwidget, mailicon,
        -- separator, upicon,     netwidget, dnicon,
        separator, fs.r.widget, fsicon,
        separator, membar.widget, memicon,
        separator, volumecfg.widget, volwidget, volbar.widget, volicon,
        separator, batwidget, baticon,
        separator, tzswidget, cpugraph.widget, cpuicon,
        separator, txwidget, separator, rxwidget,
        separator, kbdcfg.widget,
        ["layout"] = awful.widget.layout.horizontal.rightleft
    }
end
-- }}}
-- }}}


-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))

-- Client bindings
clientbuttons = awful.util.table.join(
    awful.button({ },        1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize)
)
-- }}}


-- {{{ Key bindings
--
-- {{{ Global keys
globalkeys = awful.util.table.join(
    -- {{{ Applications
    awful.key({                   }, "XF86AudioMute",         function () volumecfg.toggle()                end),
    awful.key({                   }, "XF86AudioLowerVolume",  function () volumecfg.down()                  end),
    awful.key({                   }, "XF86AudioRaiseVolume",  function () volumecfg.up()                    end),
-- Run or raise applications with dmenu
    awful.key({ modkey            }, "p",                     function () awful.util.spawn( "dmenu_run -b -p 'Run command:'" )   end),
    awful.key({ modkey            }, "t",                     function () awful.util.spawn(terminal)   end),
    awful.key({ modkey            }, "Escape",                awful.tag.history.restore),
    awful.key({ modkey, "Control" }, "r",                     awesome.restart          ),
    awful.key({ modkey, "Control" }, "l",                     function () awful.util.spawn("xlock -mode blank") end),
    awful.key({                   }, "XF86Launch1",          function () awful.util.spawn("xlock -mode blank") end),
    awful.key({ modkey            }, "o",                     awful.client.movetoscreen),

    awful.key({ winkey }, "g", function ()
        awful.prompt.run({ prompt = "Web: " }, promptbox[mouse.screen].widget,
            function (command)
                sexec("google-chrome 'http://google.com/search?q="..command.."'")
--                awful.tag.viewonly(tags[scount][8])
            end)
    end),
 
    -- {{{ Awesome controls
    awful.key({ modkey, "Shift" }, "q", awesome.quit),
    awful.key({ modkey, "Shift" }, "r", function ()
        promptbox[mouse.screen].text = awful.util.escape(awful.util.restart())
    end),
    -- }}}

    -- {{{ Tag browsing
    awful.key({ altkey, "Control" }, "n",   awful.tag.viewnext),
    awful.key({ altkey, "Control" }, "p",   awful.tag.viewprev),
    awful.key({ altkey, "Control" }, "Right",   awful.tag.viewnext),
    awful.key({ altkey, "Control" }, "Left",   awful.tag.viewprev),
    -- }}}

    -- {{{ Layout manipulation
    awful.key({ modkey }, "l",          function () awful.tag.incmwfact( 0.05) end),
    awful.key({ modkey }, "h",          function () awful.tag.incmwfact(-0.05) end),
    awful.key({ modkey, "Shift" }, "l", function () awful.client.incwfact(-0.05) end),
    awful.key({ modkey, "Shift" }, "h", function () awful.client.incwfact( 0.05) end),
    awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(layouts, -1) end),
    awful.key({ modkey },          "space", function () awful.layout.inc(layouts,  1) end),
    -- }}}

    -- switch between monitors:
--    awful.key({ modkey, "Control" }, "l", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "h", function () awful.screen.focus_relative(-1) end),

    awful.key({ modkey }, "s", function () scratch.pad.toggle() end),
    awful.key({ modkey }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey }, "F12", function () scratch.drop("urxvt", "bottom") end),

    awful.key({ modkey }, "j", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "Tab", function ()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ modkey }, "k", function ()
        awful.client.focus.byidx(-1)
        if client.focus then client.focus:raise() end
    end),
    awful.key({ altkey }, "Escape", function ()
        awful.menu.menu_keys.down = { "Down", "Alt_L" }
        local cmenu = awful.menu.clients({width=230}, { keygrabber=true, coords={x=525, y=330} })
    end),
    awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(1)  end),
    awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx(-1) end)
    -- }}}
)
-- }}}

-- {{{ Client manipulation
clientkeys = awful.util.table.join(
    awful.key({ modkey              }, "c", function (c) c:kill() end),
--    awful.key({ modkey              }, "d", function (c) scratch.pad.set(c, 0.60, 0.60, true) end),
--    awful.key({ modkey              }, "f", function (c) c.fullscreen = not c.fullscreen end),
    awful.key({ modkey, "Control"     }, "m", function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c.maximized_vertical   = not c.maximized_vertical
    end),
    awful.key({ modkey              }, "o",     awful.client.movetoscreen),
    awful.key({ winkey              }, "Next",  function () awful.client.moveresize( 20,  20, -40, -40) end),
    awful.key({ winkey              }, "Prior", function () awful.client.moveresize(-20, -20,  40,  40) end),
    awful.key({ winkey              }, "Down",  function () awful.client.moveresize(  0,  20,   0,   0) end),
    awful.key({ winkey              }, "Up",    function () awful.client.moveresize(  0, -20,   0,   0) end),
    awful.key({ winkey              }, "Left",  function () awful.client.moveresize(-20,   0,   0,   0) end),
    awful.key({ winkey              }, "Right", function () awful.client.moveresize( 20,   0,   0,   0) end),
    awful.key({ modkey, "Shift"     }, "t", function (c)
        if   c.titlebar then awful.titlebar.remove(c)
        else awful.titlebar.add(c, { modkey = modkey }) end
    end)
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
client.add_signal("manage", function (c, startup)
    -- Add titlebar to floaters, but remove those from rule callback
    if awful.client.floating.get(c)
    or awful.layout.get(c.screen) == awful.layout.suit.floating then
        if   c.titlebar then awful.titlebar.remove(c)
        else awful.titlebar.add(c, {modkey = modkey}) end
    end

    -- -- Enable sloppy focus (focus follows mouse)
     c:add_signal("mouse::enter", function (c)
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
client.add_signal("focus",   function (c) c.border_color = beautiful.border_focus  end)
client.add_signal("unfocus", function (c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Arrange signal handler
for s = 1, scount do screen[s]:add_signal("arrange", function ()
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
