cd /d %~dp0

stack build

chcp 65001 & stack exec site rebuild & chcp 932

chcp 65001 & stack exec site watch
