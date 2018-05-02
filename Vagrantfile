VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  if ENV['VAGRANT_OS'] == 'mac'
    config.vm.box = "osx"
    config.ssh.username = "vagrant"
    config.ssh.password = "vagrant"
    config.vm.provider "virtualbox" do |vb|
      config.vm.synced_folder ".", "/vagrant", type: "rsync"
    end
  end

  if ENV['VAGRANT_OS'] == 'rh'
    config.vm.box = "centos/7"
  end

  if ENV['VAGRANT_OS'] == 'deb'
    config.vm.box = "debian/jessie64"
  end

  config.vm.define "host1" do |host1|
    host1.vm.hostname = "host1"
    host1.vm.network :private_network, ip: "192.168.33.39"
  end

end
