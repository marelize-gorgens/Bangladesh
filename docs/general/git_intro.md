# Short Intro to Git

## Tutorials

First here are some short/general intro tutorials (both video and textual) for git presenting some basic command that you will need.

1. [Getting Started Git Basics](https://git-scm.com/book/en/v2/Getting-Started-Git-Basics)
2. [Git Guide](http://rogerdudler.github.io/git-guide/)
3. [Very Short Learn Git Course](https://www.codecademy.com/learn/learn-git)
4. [Basic Git Tutorial Video 1](https://www.youtube.com/watch?v=SWYqp7iY_Tc)
5. [Basic Git Tutorial Video 2](https://www.youtube.com/watch?v=HVsySz-h9r4)

## General Procedure:

The usual procedure when working with git in terminal would be following:
- Open your command line:
- Run the following command to get the latest things from “master” branch:
```
git pull origin master
```
- Check available branches with:
```
git branch
```

- Create your own branch for the first time and pull everything from the master branch:
```
git checkout -b name_of_your_branch
git pull origin master
```

- Once you created your branch you will just switch from master branch to your using
```
git checkout name_of_your_branch
``` 
and to go back to master branch  use command
```
git checkout master
```
- Once you are at your branch you can work edit, work, change your files and it won’t affect the master branch. To be sure that your on what branch you are just type the below command and and you will see name of branch with asterisk that indicate at what branch you are.
```
git branch
```
- Once your finished with your editing and want to save/push stuff to your branch just use the following procedure:
  - Sanity check if you are at your branch:
  ```
  git branch
  ```
  - Check what files are changes:
  ```
  git status
  ```
  - You will see the message for names of files and directories in red that are add/modified/rm and to add those to your branch do the following:
    - This adds all files:
    ```
    git add * 
    ```
    or add individual files with
    ```
    git add name_of_file
    ```
    - After you added files you want to push to your branch the next step is to commit those with:
    ```
    git commit -m ‘My latest changes’
    ```
    -m stands for message you want to leave such as ‘My latest changes’ to know what have you done.
    - After this the final step is to push your commit with:
    ```
    git push origin name_of_your_branch
    ```
 
