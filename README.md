## Realtime information system

<a href="https://assembly.com/ccar-websockets/bounties?utm_campaign=assemblage&utm_source=ccar-websockets&utm_medium=repo_badge"><img src="https://asm-badger.herokuapp.com/ccar-websockets/badges/tasks.svg" height="24px" alt="Open Tasks" /></a>

#### Realtime information system


This is a product being built by the Assembly community. You can help push this idea forward by visiting [https://assembly.com/ccar-websockets](https://assembly.com/ccar-websockets).

#### How Assembly Works

Assembly products are like open-source and made with contributions from the community. Assembly handles the boring stuff like hosting, support, financing, legal, etc. Once the product launches we collect the revenue and split the profits amongst the contributors.

Visit [https://assembly.com](https://assembly.com) to learn more.
=======


#### Beta. 


Compilers/libraries
================
The versions listed are the ones on my host. Most of the chnages will be backward compatible, so if you 
have downloaded a leter version, things will most likely work.
* ghci: > 7.6.3 && < 7.7
* cabal:  > 1.20 && < 1.21

* haxe : 3.0.0


* Haxe Libraries:
* mlib: [2.0.2]
* munit: [2.1.0]
* mconsole: [1.6.0]
* promhx: [1.0.16]
* hamcrest: [1.2.1]
* mcover: [2.1.1]


Installation of the yesod toolchain
====================================
* $ cabal sandbox init
* $ cabal install yesod-core --max-backjumps=-1 --reorder-goals --reinstall


Installation using stack
======================================
wget -q -O- https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
echo 'deb http://download.fpcomplete.com/ubuntu/precise stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
sudo apt-get update && sudo apt-get install stack -y


Some admin commmands
====================================
sudo -u postgres psql -d <dbname>

Login as root
kill -QUIT $(cat /usr/local/nginx/logs/nginx.pid)
#Starting stunnel if installed:
root@koala:/etc/stunnel# /etc/init.d/stunnel4 restart

Self signing certificates
  Url [https://www.digitalocean.com/community/tutorials/how-to-set-up-an-ssl-tunnel-using-stunnel-on-ubuntu]
  openssl genrsa -out /etc/stunnel/key.pem 4096
  openssl req -new -x509 -key /etc/stunnel/key.pem -out /etc/stunnel/cert.pem -days 1826

To find out which version of ubuntu I am running
 lsb_release -a 
