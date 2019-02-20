## Requirements

If you don't have already installed Python and/or Jupyter Notebook. The easiest way to have all necessary tools for using this notebook is to install [Anaconda](https://www.anaconda.com/download/) Python distribution (__Python 3.6 version__)  which includes Python, Jupyter Notebook and many additional data science tools.

[Anaconda](https://www.anaconda.com/) is a free and open source distribution of the Python and R programming languages for large-scale data processing, predictive analytics, and scientific computing, that aims to simplify package management and deployment and includes wide-range of data science tools including Jupyter Notebook.

[The Jupyter Notebook](http://jupyter.org/) is an open-source web application that allows you to create and share documents that contain live code, equations, visualizations and narrative text. Uses include: data cleaning and transformation, numerical simulation, statistical modeling, data visualization, machine learning, and much more.

### Installing Anaconda on MacOS

For detailed instructions on how to install it on MacOS machines can be found [here](https://conda.io/docs/user-guide/install/macos.html).

1. Download the installer from [here (miniconda)](https://conda.io/miniconda.html) or from [here (anaconda)](https://www.anaconda.com/download/#macos). __Download Python 3.6 version.__
2. Install:
   * Miniconda. Run the following commmand in your Terminal window:
      ```
      bash Miniconda3-latest-MacOSX-x86_64.sh
      ```
   * Anaconda:   
   Double-click the .pkg file. Follow the prompts on the installer screens.    
   If you are unsure about any setting, accept the defaults. You can change them later. To make the changes take effect, close and then re-open your Terminal window. 
3. Test your installation by going through the series of the following [tests](https://conda.io/docs/user-guide/install/test-installation.html).


### Installing Anaconda on Linux

For detailed instructions on how to install it on Linux machines can be found [here](https://conda.io/docs/user-guide/install/linux.html).

1. Download the installer for Linux from [here (miniconda)](https://conda.io/miniconda.html) or from [here (anaconda)](https://www.anaconda.com/download/#linux). __Download Python 3.6 version.__
2. Install:
   * Miniconda. Run the following commmand in your Terminal window:
      ```
      bash Miniconda3-latest-MacOSX-x86_64.sh
      ```
   * Anaconda:
      ```
      bash Anaconda-latest-Linux-x86_64.sh
      ```
   Follow the prompts on the installer screens.    
   If you are unsure about any setting, accept the defaults. You can change them later. To make the changes take effect, close and then re-open your Terminal window. 
3. Test your installation by going through the series of the following [tests](https://conda.io/docs/user-guide/install/test-installation.html)


### Installing Anaconda on Windows

For detailed instructions on how to install it on Windows machines can be found [here](https://conda.io/docs/user-guide/install/windows.html). __Download Python 3.6 version.__

1. Download the installer for Windows from [here (miniconda)](https://conda.io/miniconda.html) or from [here (anaconda)](https://www.anaconda.com/download/#windows)
2. Double-click the .exe file. Follow the instructions on the screen. If you are unsure about any setting, accept the defaults. You can change them later. When installation is finished, from the Start menu, open the Anaconda Prompt. 
3. Test your installation by going through the series of the following [tests](https://conda.io/docs/user-guide/install/test-installation.html)


### Updating missing Python packages through "conda" package manager

In case you get messages that you don't have installed some packages within your Anaconda Python distribution, you can install those missing packages using ["conda"](https://conda.io/docs/) package manager.
For example to install a conda PyMongo package type the below command in your Terminal window or Anaconda Prompt run:

 ```
 conda install -c anaconda pymongo
 ```
Another option is to install missing packages through Anaconda Navigator. There is animated gif below.

![img](https://media.giphy.com/media/Eqc6V50w5e6ffksiGS/giphy.gif)


### Updating missing Python packages through "pip" package manager

Detailed description on how to install Python packages using pip package manager can be found [here](https://docs.python.org/3/installing/index.html)


### Installing Jupyter Notebook instead of Anaconda using pip

If you don't want to install Anaconda and already have Python, you can install Jupyter using Python’s package manager, pip, instead of Anaconda. First, ensure that you have the latest pip; older versions may have trouble with some dependencies:
  ```
  pip3 install --upgrade pip
  ```
Then install the Jupyter Notebook using:
  ```
  pip3 install jupyter
  ```
Start the notebook server from the command line:
```
jupyter notebook
```
You should see the notebook open in your default browser.

### Launching Jupyter Notebook if you have installed Anaconda

#### On MacOS and Linux

The Jupyter Notebook App can be launched by typing in Terminal:
  ```
  jupyter notebook
  ```

#### On Windows

The Jupyter Notebook App can be launched by clicking on the Jupyter Notebook icon installed by Anaconda in the start menu (Windows) or by typing in a terminal (cmd on Windows):
  ```
  jupyter notebook
  ```

#### After launching

After launching your Jupyter Notebook a new browser window (or a new tab) showing the Notebook Dashboard will open. Notebook Dashboard is a sort of control panel that allows (among other things) to select which notebook to open.

__IMPORTANT: When started, the Jupyter Notebook App can access only files within its start-up folder (including any sub-folder). No configuration is necessary if you place your notebooks in your home folder or subfolders. Otherwise, you need to choose a Jupyter Notebook App start-up folder which will contain all the notebooks.__


#### More about Jupyter Notebook

You can find more info on how to run Jupyter Notebook [here](http://jupyter-notebook-beginner-guide.readthedocs.io/en/latest/execute.html) and [here](http://jupyter.readthedocs.io/en/latest/running.html#running) and for some more tricks and tips [here](https://www.dataquest.io/blog/jupyter-notebook-tips-tricks-shortcuts/) and [here](http://arogozhnikov.github.io/2016/09/10/jupyter-features.html). There is also this [tutorial](https://www.datacamp.com/community/tutorials/tutorial-jupyter-notebook) on Jupyter Notebook created by [DataCamp](https://www.datacamp.com/).


#### Converting Jupyter Notebook to slides

In case you would like to convert your Jupyter Notebook to slide please follow this [detailed tutorial](https://medium.com/@mjspeck/presenting-code-using-jupyter-notebook-slides-a8a3c3b59d67) on how to do it.
