# Jira Emacs
My personal Emacs Jira client. 

# Install

## Download Code

git clone git@github.com:nav/jira-emacs.git
mv jira-emacs/modules ~/.config/doom

## Use Package

**Authinfo**

Create ~/.authinfo with the following information

`machine <YOUR-DOMAIN>.atlassian.net login <EMAIL> password <API-TOKEN> port 443`

**Change Init**

In .config/doom/init.el, Add "jira" to `:tools`

**Change Config**

(setq +jira-host "https://<your-subdomain>.atlassian.net")


## Run

`./config/emacs/bin/doom sync`



# Usage

C-c j - Jira

C-c j d - Turn on Debugging
C-c j s - Assigned issues in current sprint
C-c j i - Find an issue by id
C-c j f - Search for issues
