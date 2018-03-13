# traffic-dynamics-shiny
This project implements a shiny-app in a Docker image that visualizes automatic traffic countings from Münster (Westf.), Germany.
The data are downloaded, pre-processed, and put in a Docker image [in this repository](https://github.com/codeformuenster/traffic-dynamics).
So, this Docker-based shiny-app implementation downloads the [corresponding Docker image](https://hub.docker.com/r/codeformuenster/traffic-dynamics/) and adds shiny-relevant source code.

## How to start

### Without docker

This shiny app assumes that the folder `shinyapp/data/database` contains a proper sqlite database. This database is created by the progam in our [traffic-dynamics repo](https://www.github.com/codeformuenster/traffic-dynamics).
If you have the database available, you can create a symlink to it.

```
cd shinyapp
mkdir data
ln -s whereever/you/stored/the/data/database database
```

Then you can start this shiny app for instance by using RStudio.


### With docker

Make sure to rename or delete the local symlink to the database you created in the step above, otherwise, the docker image won't run.
The following commands start the docker daemon, build a local docker image and run this local image:

```
sudo systemctl start docker
sudo docker build -t traffic-dynamics-local .
sudo docker run --rm -p 3838:3838 traffic-dynamics-local
```

Alternatively, you can download and run the docker image from the docker cloud:
```
sudo docker run --rm -p 3838:3838 codeformuenster/traffic-dynamics-shiny:latest
```

Afterwards, point your browser to [localhost:3838](localhost:3838).

## Contributors

* [silberzwiebel](https://github.com/silberzwiebel)
* [ThorbenJensen](https://github.com/ThorbenJensen)

## Rechtliches

### Quelltext

Copyright © 2017-2018 Thorben Jensen, Thomas Kluth

#### Deutsch 

Dieses Programm ist Freie Software: Sie können es unter den Bedingungen
der GNU General Public License, wie von der Free Software Foundation,
Version 3 der Lizenz oder (nach Ihrer Wahl) jeder neueren
veröffentlichten Version, weiterverbreiten und/oder modifizieren.

Dieses Programm wird in der Hoffnung, dass es nützlich sein wird, aber
OHNE JEDE GEWÄHRLEISTUNG, bereitgestellt; sogar ohne die implizite
Gewährleistung der MARKTFÄHIGKEIT oder EIGNUNG FÜR EINEN BESTIMMTEN ZWECK.
Siehe die GNU General Public License für weitere Details.

Sie sollten [eine Kopie der GNU General Public License zusammen mit diesem
Programm erhalten haben](COPYING). Wenn nicht, siehe <http://www.gnu.org/licenses/>.

#### Englisch

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have [received a copy of the GNU General Public License
along with this program](COPYING). If not, see <http://www.gnu.org/licenses/>.

### Daten

Datenquelle: Stadt Münster

[Datenlizenz Deutschland – Namensnennung – Version 2.0](http://www.govdata.de/dl-de/by-2-0) (oder [diese pdf-Datei](doc/Stadt_MS_OpenData_Datenlizenz_Deutschland.pdf))
