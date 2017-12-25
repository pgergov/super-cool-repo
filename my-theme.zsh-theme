# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png
eval my_blue='$fg[blue]'
eval my_green='$fg[green]'
eval my_red='$fg[red]'
eval my_yellow='$FG[136]'

local return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"
local user_host='%{$my_green%}%n%{$reset_color%}'
local current_dir='%{$my_blue%}%~%{$reset_color%}'
local user_symbol='♥'
local git_branch='$(git_prompt_info)%{$reset_color%}'

# local current_time='%{$my_green%}%T%{$reset_color%}'
# local virtualenv='%{$my_purple%}(virtualenv_prompt_info)%{$reset_color%} '

PROMPT="${user_host} ${current_dir} ${git_branch}
%B${user_symbol}%b "
RPS1="${return_code}"

ZSH_THEME_GIT_PROMPT_PREFIX="%{$my_red%}‹"
ZSH_THEME_GIT_PROMPT_SUFFIX="› %{$reset_color%}"
