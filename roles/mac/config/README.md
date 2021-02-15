Additionally, we can do next steps.

## Editorconfig

Use it for your project.

```
root = true

[*]
end_of_line = lf
insert_final_newline = true

[*.js]
charset = utf-8
indent_style = space
indent_size = 2

# 4 space indentation
[*.py]
indent_style = space
indent_size = 4

# Tab indentation (no size specified)
[Makefile]
indent_style = tab

# Indentation override for all JS under lib directory
[lib/**.js]
indent_style = space
indent_size = 2

# Matches the exact files either package.json or .travis.yml
[{package.json,.travis.yml}]
indent_style = space
indent_size = 2
```

## Appearance

* To open definition in new tab use coc-settings.json.

```
touch ~/.config/nvim/coc-settings.json
```

## Coc extensions

```
:CocInstall coc-metals coc-json coc-tsserver coc-eslint coc-json coc-prettier coc-css coc-jedi coc-xml coc-yaml coc-sql coc-stylelint coc-sh coc-markdownlint coc-java coc-html coc-highlight coc-go coc-git coc-fzf-preview coc-cmake coc-conjure
```

#### Eslint
```
npm install eslint --save-dev
npm install -D prettier eslint-plugin-prettier eslint-config-prettier
npm init
./node_modules/.bin/eslint --init
```
