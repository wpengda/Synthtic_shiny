## Synthetic data shiny app

Welcome to this guide on how to get started with this synthetic data generation Shiny app using RStudio. Shiny is a powerful R package that allows you to create interactive web applications directly from R. This guide assumes you have RStudio installed on your computer. If not, please install it from the [RStudio Download Page](https://rstudio.com/products/rstudio/download/).

## Prerequisites

Before we start, ensure you have the following:

- RStudio installed on your computer.
- Anaconda installed for managing virtual environments.

## Step 1: Setting Up RStudio

1. **Launch RStudio**: Open RStudio to begin setting up your environment for Shiny apps.
2. **Install Package**: You need to install the packages used in this shiny app if you haven't already. You can do this by running the following command in the R console:

```R
install.packages("shiny")
install.packages("shinyjs")
install.packages("MASS")
install.packages("DT")
install.packages("reticulate")
install.packages("ggplot2")
install.packages("shinycssloaders")
install.packages("moments")
```

## Step 2: Setting Up Anaconda Virtual Environment

Anaconda is a powerful tool for managing virtual environments and dependencies. Shiny apps can be run in isolated environments to manage package versions and dependencies efficiently.

1. **Install Anaconda***: To download and install Conda on Windows or macOS, you'll typically use Miniconda or Anaconda, which are free distributions that include Conda, Python, and over 150 scientific packages and their dependencies.

**Visit the Official Anaconda Website**: Go to the Anaconda distribution page ([https://www.anaconda.com/products/individual](https://www.anaconda.com/products/individual)) to download Anaconda or go to the Miniconda page ([https://docs.conda.io/en/latest/miniconda.html](https://docs.conda.io/en/latest/miniconda.html)) to download Miniconda. Anaconda includes a lot of pre-installed packages which is useful for scientific computing, data science, and machine learning tasks. Miniconda is a minimal installer for Conda and you install the packages you need manually.

**Choose the Installer**: Select the Windows version of the installer. For Anaconda, you can choose between a graphical installer and a command-line installer. For Miniconda, download the appropriate .exe file.

**Run the Installer**: Once downloaded, double-click the installer to start the installation. Follow the instructions on the screen. It's recommended to check the option that adds Conda to your PATH environment variable, although the installer advises against it for beginners.

**Verify Installation**: Open the Command Prompt (cmd) and type `conda --version`. If Conda is installed, you should see the version of Conda printed on the screen.


2. **Create a New Virtual Environment**: After installing, you can use the `conda` command to manage packages and environments. For example, to create a new environment named `myenv` with Python 3.10, you would use:

```bash
conda create --name myenv python=3.10
```

3. **Activate the Virtual Environment**: Once the environment is created, activate it using:

```bash
conda activate myenv
```

## Step 3: Install Python package and find the Python path

Now in the Command Prompt (cmd) you should see something like:

```bash
(myenv) C:\Users\Desktop\synthtic_data_shiny>
```
1. **Install Python package**: We can start install some Python package we need in this environment:

```bash
pip install sdv
```

```bash
pip install pandas
```

```bash
pip install argparse
```

2. **Find Python path**: Then use `where python` to find python path:

```bash
where python
```
You should have somoe line like:

```bash
C:\Users\anaconda3\envs\myenv\python.exe
C:\Users\anaconda3\python.exe
C:\Users\AppData\Local\Microsoft\WindowsApps\python.exe
```
We gonan use the first path `C:\Users\anaconda3\envs\myenv\python.exe`.


3. **Set Python path in R**: Then we open the app.R find line **196**

```R
python_path <- "python3"
```

We change this to: 

```R
python_path <- "C:/Users/anaconda3/envs/myenv/python.exe" 
```

Remember we need change \ to /.

## Step 4: Running the Shiny App in RStudio

1. **Open the Shiny App**: In RStudio, go to File > open file > choose the app.R

2. **Run Your App**: Click the 'Run App' button in RStudio to launch your Shiny app. RStudio will automatically open a web browser displaying your app.

## Step 5: Additional Resources

For more detailed guidance on building Shiny apps, including advanced user interface design, server logic, and deploying your apps, visit the official Shiny website at [shiny.rstudio.com](https://shiny.rstudio.com).

For Python package we use:
[1] @inproceedings{ctgan,
  title={Modeling Tabular data using Conditional GAN},
  author={Xu, Lei and Skoularidou, Maria and Cuesta-Infante, Alfredo and Veeramachaneni, Kalyan},
  booktitle={Advances in Neural Information Processing Systems},
  year={2019}
}

[2] @inproceedings{
    SDV,
    title={The Synthetic data vault},
    author={Patki, Neha and Wedge, Roy and Veeramachaneni, Kalyan},
    booktitle={IEEE International Conference on Data Science and Advanced Analytics (DSAA)},
    year={2016},
    pages={399-410},
    doi={10.1109/DSAA.2016.49},
    month={Oct}
}
