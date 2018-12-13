## Ansible Bootstrap

Bootstrap your environment

## Mac OS X Vagrant Box preparation

You need to prepare virtual image which will be used by vagrant manually. One of the ways is described here.

1) Use this https://github.com/timsutton/osx-vm-templates to prepare iso/vdi

```
git clone https://github.com/timsutton/osx-vm-templates
cd osx-vm-templates
sudo prepare_iso/prepare_vdi.sh /Applications/InstallmacOSHighSierra.app/ packer/ ## or other macos installation package
```

It will prepare virtual disk image in dest dir:
```
▶ ls packer/
macOS_10.13.3.vdi     macOS_10.13.3.vdi.md5
```

2) Create new virtual machine in Virtual Box with name "OS X" (or any other) and boot it

3) Shutdown vm in Virtual Box and prepare package for vagrant

```
vagrant package --base "OS X"
```

It will prepare package.box (name 'package' by default) in current directory.

4) Add box for vagrant

```
vagrant box add --name osx package.box
```

With rh/deb we use ready boxes from vagrant repo.

## Testing with Vagrant

### Mac OS X

```
VAGRANT_OS='mac' vagrant up
ansible-playbook -i hosts-vagrant main.yml --extra-vars "role=mac"
```

### Red Hat like Unix

```
VAGRANT_OS='rh' vagrant up
ansible-playbook -i hosts-vagrant main.yml --extra-vars "role=rh"
```

### Debian like Unix

```
VAGRANT_OS='deb' vagrant up
ansible-playbook -i hosts-vagrant main.yml --extra-vars "role=deb"
```

### TODO

* install brew
* install ansible to install this shit
