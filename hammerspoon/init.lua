caffeine = hs.menubar.new()

function setCaffeineDisplay(state)
    if state then
        caffeine:setTitle("😳")
    else
        caffeine:setTitle("😴")
    end
end
function caffeineClicked()
    setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end
if caffeine then
    caffeine:setClickCallback(caffeineClicked)
    setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end
function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Config loaded")

local switcher_space = hs.window.switcher.new(
    hs.window.filter.new():setCurrentSpace(true):setDefaultFilter{}
) -- include minimized/hidden windows, current Space only

hs.hotkey.bind({"alt"}, "l", function()
    switcher_space:next()
end)

hs.hotkey.bind({"command", "control"}, "h", function()
    -- log
    local screen = hs.mouse.getCurrentScreen():next()
    local windows = hs.window.orderedWindows()
    local found = false
    for _, win in ipairs(windows) do
        if win:screen() == screen then
            win:focus()
            hs.alert.closeAll()
            hs.alert.show(win:title(), {
                atScreenEdge = 0,
                fadeInDuration = 0.0,
                fadeOutDuration = 0.2,
                padding=20.0,
            }, hs.screen.mainScreen(), 0.4)
            local screenSize = win:screen():frame()
            hs.mouse.setRelativePosition({x= screenSize.w/2, y= screenSize.h/2}, screen)
            found = true
            break
        end
    end
    if found == false then
        local screenSize = screen:frame()
        hs.mouse.setRelativePosition({x=screenSize.w/2, y=screenSize.h/2}, screen)
        hs.alert.show("SWITCHED", {
            atScreenEdge = 0,
            fadeInDuration = 0.0,
            fadeOutDuration = 0.2,
            padding=20.0,
        }, hs.mouse.getCurrentScreen(), 0.4)
    end
end)

hs.hotkey.bind({"cmd", "shift"}, "M", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h
    win:setFrame(f)
end)

-- Right side
hs.hotkey.bind({"cmd", "shift"}, "l", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    if f.w == max.w / 2 and f.x == max.x + (max.w / 2) then
        f.w = max.w / 4
        f.x = max.x + (3 * max.w / 4)
    else
        f.w = max.w / 2
        f.x = max.x + (max.w / 2)
    end
    f.y = max.y
    f.h = max.h
    win:setFrame(f)
end)
-- Left side
hs.hotkey.bind({"cmd", "shift"}, "h", function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    if f.w == max.w / 2 then
        f.w = 3 * max.w / 4
    else
        f.w = max.w / 2
    end
    f.x = max.x
    f.y = max.y
    f.h = max.h
    win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "control"}, "l", function()
    -- lock screen
    hs.caffeinate.lockScreen()
end)
