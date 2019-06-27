print(c.bindings.commands)

c.tabs.position = "left"
c.tabs.show = "multiple"
c.auto_save.session = True
c.completion.use_best_match = True
c.content.cookies.store = False
c.content.geolocation = False
c.editor.command = [ "emacsclient", "-c", "-e", "(progn (find-file \"{file}\") (goto-line {line}) (beginning-of-line) (forward-char (- {column} 1)))" ]
c.downloads.location.directory = "/home/jsrn/downloads/tmp/"
c.downloads.location.prompt = False
c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.106 Safari/537.36"

config.bind('n', 'run-with-count 2 scroll down')
config.bind('e', 'run-with-count 2 scroll up')
config.bind('y', 'scroll left')
config.bind('o', 'scroll right')

config.bind('O', 'set-cmd-text -s :open ')
config.bind('t', 'set-cmd-text -s :open -t ')

config.bind('s', 'tab-prev')
config.bind('h', 'tab-next')
# <Alt-Num> for jumping to tab Num
config.bind('h', 'tab-next')
config.bind('xt', 'scroll right')

config.bind('<Ctrl-k>', 'tab-clone')
config.bind('<Ctrl-O>', 'back')
config.bind('<Ctrl-F>', 'forward')
config.bind('<Ctrl-I>', 'forward')

config.bind('k', 'search-next')
config.bind('K', 'search-prev')

config.bind('<Ctrl-i>', 'open-editor')
config.bind('<Ctrl-i>', 'open-editor', mode="insert")

config.bind('j', 'yank pretty-url')

config.bind('<Ctrl-0>', 'zoom 100')


config.bind(',m', "spawn mpv {url}")
config.bind(',M', "hint links spawn mpv {hint-url}")

config.bind(',z', "jseval var d=document,s=d.createElement('script');s.src='https://www.zotero.org/bookmarklet/loader.js';(d.body?d.body:d.documentElement).appendChild(s);void(0);")


# tabbar
def makePadding(top, bottom, left, right):
    return { 'top': top, 'bottom': bottom, 'left': left , 'right': right }
c.tabs.padding = makePadding(1,1,8,8)
c.tabs.indicator.padding = makePadding(0,0,0,0)
