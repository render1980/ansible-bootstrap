Additionally, we might to do next steps.

## Editorconfig

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

## Scala

```
:ConInstall coc-metals
```

## Javascript

```
:CocInstall coc-json coc-tsserver coc-eslint coc-json coc-prettier coc-css
```

#### Eslint
```
npm install eslint --save-dev
npm init
./node_modules/.bin/eslint --init
```
