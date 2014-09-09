ddgatve-screenscrapers
======================

A project to extract election data for Latvian parliament Saeima.

How to run project on Windows
================================
(1) Install Git; JDK 1.7; Eclipse
(2) If you are behind a proxy, run this command: 
git config --global http.proxy http://your.proxy.com:8081
(3) Clone the repository - run this command: 
git clone https://github.com/kapsitis/ddgatve-screenscrapers.git
(4) Download Typesafe Activator from https://typesafe.com/platform/getstarted
Select "Minimal package". Unzip. 
Move the files "activator", "activator.bat" and "activator-launch-1.2.10.jar" 
to the root directory of ddgatve-screenscrapers
(5) Fix the dependencies. Change directory to ddgatve-screenscrapers. Run command: 
activator eclipse
