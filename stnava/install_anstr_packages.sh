#!/bin/bash
myos=`uname`
RPDIR=/tmp/ # just put the packages into a tmp directory 
RLDIR=$1 # library directory
INSTALLR=0
INSTALLRPKG=0
if [[ ${#RLDIR} -le 0 ]]  ; then 
  echo "This will install R and ANTsR as well - if you want only ANTsR then set 2nd / 3rd argument to zero "
  echo " but we assume you installed a compatible version of R."
  echo "usage is : "
  echo $0  ANTSRDIR 1_or_0_R   1_or_0_R_pkgs
  echo  where ANTSRDIR is an absolute path where you want to put R packages and ANTsR
  echo  the 2 booleans control whether you install R or R_pkgs that are needed by ANTsR
  echo  $0  ~/RLibraries/ 0 1 
  exit 1 
fi
if [[ $# -gt 1 ]] ; then 
  INSTALLR=$2
fi
if [[ $# -gt 2 ]] ; then 
  INSTALLRPKG=$3
fi
echo will install R? $INSTALLR
echo will install R_PKG? $INSTALLRPKG
if [[ $myos == "Linux" ]] && [[ $INSTALLR -gt 0 ]] ; then
  sudo apt-get install build-essential git subversion cmake-curses-gui xorg libx11-dev freeglut3 freeglut3-dev
  if [[ $INSTALLR -gt 1 ]] ; then 
    sudo apt-get  r-base r-base-dev 
  fi
fi 
#
if [[ $myos == "Darwin" ]]  && [[ $INSTALLR -ge 1 ]] ; then
# get homebrew 
  ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
  brew update
  brew prune
  brew install wget 
  brew install git 
  brew install gfortran 
  brew install CMake 
  if [[ ${#R_LD_LIBRARY_PATH} -gt 0 ]] ; then 
    echo "R_LD_LIBRARY_PATH should not be set.  This may cause R installation problems."
  fi
  if [[ $INSTALLR -gt 1 ]] ; then 
    brew install R
  fi
  if [[ -s ~/.profile ]] ; then 
    echo 'export PATH="/usr/local/bin:/usr/local/sbin:~/bin:$PATH"' >> ~/.profile
  else 
    echo 'export PATH="/usr/local/bin:/usr/local/sbin:~/bin:$PATH"' >> ~/.bash_profile
  fi
# set these correctly for a pure homebrew install .... 
#  RLDIR=/usr/local/opt/r/R.framework/Libraries/
  if [[ ! -s $RLDIR ]] ; then 
    mkdir -p $RLDIR 
  fi 
  echo i am using the homebrew R library directory $RLDIR 
fi
command -v wget >/dev/null 2>&1 || { echo >&2 "I require wget but it's not installed.  Aborting."; exit 1; }
command -v git >/dev/null 2>&1 || { echo >&2 "I require git but it's not installed.  Aborting."; exit 1; }
command -v cmake >/dev/null 2>&1 || { echo >&2 "I require cmake but it's not installed.  Aborting."; exit 1; }
command -v R >/dev/null 2>&1 || { echo >&2 "I require R but it's not installed.  Aborting."; exit 1; }  
echo "Grabbing and installing ANTsR dependencies"

if [ ! -d "$RPDIR" ]; then
  mkdir $RPDIR
fi

if [ ! -d "$RLDIR" ]; then
  mkdir $RLDIR
fi

cd $RPDIR
if [[ $INSTALLRPKG == 1 ]] ; then 
pkgs=( fit.models_0.5-10.tar.gz robustbase_0.9-10.tar.gz mvtnorm_0.9-9993.tar.gz pcaPP_1.9-48.tar.gz rrcov_1.3-02.tar.gz robust_0.4-15.tar.gz
boot_1.3-8.tar.gz	 timeDate_2160.97.tar.gz iterators_1.0.6.tar.gz foreach_1.4.0.tar.gz signal_0.7-2.tar.gz timeSeries_2160.95.tar.gz Rcpp_0.10.3.tar.gz mFilter_0.1-3.tar.gz doParallel_1.0.1.tar.gz abind_1.4-0.tar.gz magic_1.5-4.tar.gz stringr_0.6.2.tar.gz evaluate_0.4.3.tar.gz digest_0.6.3.tar.gz formatR_0.7.tar.gz markdown_0.5.4.tar.gz knitr_1.1.tar.gz pixmap_0.4-11.tar.gz rgl_0.93.928.tar.gz misc3d_0.8-4.tar.gz);
names=( fit.models robustbase mvtnorm pcaPP rrcov robust boot timeDate iterators foreach signal timeSeries Rcpp mFilter doParallel abind magic stringr evaluate digest formatR markdown knitr pixmap rgl misc3d);
######## now install R packages ##########
counter=0
for pkg in ${pkgs[*]} ; do
  wget http://cran.r-project.org/src/contrib/$pkg
  if [ ! -s $pkg ]; then
    wget http://cran.r-project.org/src/contrib/Archive/${names[$counter]}/${pkg}
  fi
  echo install ${names[$counter]} to $RLDIR 
  R CMD INSTALL -l $RLDIR $pkg 
  let counter=counter+1
done
fi 


ANTSRDIR=$RLDIR
cd $ANTSRDIR
echo install ANTsR to $ANTSRDIR 
MYRLDIR=${ANTSRDIR}/ANTsR_src/ANTsR/src/ANTS/ANTS-build/lib
RHLIB=` R RHOME`/lib
echo R home lib is $RHLIB 
# if [[ ${#R_LD_LIBRARY_PATH} -eq 0 ]] ; then 
  mybashstring=`echo export R_LD_LIBRARY_PATH=${MYRLDIR}:${RHLIB}:${R_LD_LIBRARY_PATH}:${LD_LIBRARY_PATH} `
  if [[ -s ~/.profile ]] ; then 
    echo $mybashstring >> ~/.profile
    source ~/.profile
  else 
    echo ${mybashstring} >> ~/.bash_profile
    source ~/.bash_profile
  fi 
# else 
  echo "WARNING:  added $mybashstring to your environment R_LD_LIBRARY_PATH"
# fi
echo  check ${ANTSRDIR}/ANTsR_src/ANTsR
if [[ ! -s ${ANTSRDIR}/ANTsR_src/ANTsR ]] ; then 
  mkdir ANTsR_src
  cd ANTsR_src
  echo clone ANTsR
  git clone http://github.com/stnava/ANTsR.git
fi 
cd  ${ANTSRDIR}/ANTsR_src
echo call R CMD INSTALL now ... 
R CMD INSTALL -l $RLDIR  ANTsR
myrenvstring=`echo R_LIBS_USER=${ANTSRDIR}:${MYRLDIR}:\$\{R_LIBS_USER\} `
echo ${myrenvstring} >> ~/.Renviron



