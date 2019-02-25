echo README.md package.yaml app/Main.hs src/HubBoard/Command/Desc.hs | xargs sed -i.bak "s/$1/$2/g"

rm README.md.bak package.yaml.bak app/Main.hs.bak src/HubBoard/Command/Desc.hs.bak

stack build --test --copy-bins --local-bin-path publish/bin
