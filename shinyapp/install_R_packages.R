# This program is free software.
# You should have received a copy of the GNU General Public License
# along with this program (file COPYING). If not, see <http://www.gnu.org/licenses/>.

packages <- c("shiny", "datasets", "RSQLite", "dplyr", "ggplot2", "lubridate", "plotly", "brms")

installed_packages <- installed.packages()[,"Package"]

needed_packages <- packages[!(packages %in% installed_packages)]

if (length(needed_packages) > 0) {
  install.packages(needed_packages)
}
