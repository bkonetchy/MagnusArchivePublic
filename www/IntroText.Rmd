---
title: "Magnus Archives Dashboard User Manual"
author: "Brant Konetchy"
date: "8/23/2020"
editor_options: 
  markdown: 
    wrap: 72
output: 
  html_document: 
    keep_md: yes
---

# Introduction

Magnus Archives dashboard was developed to allow listeners of the
[Magnus Archives Podcast](http://rustyquill.com/the-magnus-archives/),
to follow along with the podcast and develop their own research and
notes into the cases. The Help tab will contain all the information
needed to use and understand the dashboard and is the primary place to
get started using the dashboard.

# Dashboard Sections

The dashboard consists of four sections.

-   Map
-   Timeline
-   Help
-   Database
    -   Log in DB
    -   Data Input

## Map

Map section contains an interactive map with all cases marked. The map
is interactive and you can zoom in and out, pan, and click on each
marker to obtain the name with hyperlink to audio, case file number, and
episode number. In addition to displaying the markers and additional
query box is shown at the top of the map. From the box any case file
name can be selected to show only that case file, multiple case files
can be selected, or no case files selected to show all locations.
Potential updates are to allow for searching based on distances from
each case, such as all cases within 10 km from one another.

## Timeline

The timeline section contains a graph showing the timelines of each case
file. The graph is interactive and cases can be turned off/on from the
legend as well as expanding the both axis and zooming into sections. For
a full understanding of the graph capabilities click on the link on the
top right hand side of the graph to go to ploty website to learn more.
The graph itself shows time on the x axis with the episode number shown
on the y axis. For cases in which the events took place over a period of
time, a bar will be drawn from the start to end points. Events occurring
on a single day will be shown as a point. For cases in which the exact
dates are know will be solid, and those that are unknown will be dashed
to indicate the uncertainty in the dates.

## Help

Help (current) document.

## Database

The database section consists of two sub-sections. The first allows
authorized users to log in to access and alter the database, and the
second is for adding, editing, and deleting data from the database. The
Data Input section consists of five different tables: Case Files, Event
Locations, Persons of Interest, Magnus Events, and Connected Cases. For
the first table, Case Files, the add button can be used to bring up a
form to fill out with the Case File information, the rest of the tables
have the form information directly on the page. For all tables the edit
and delete button function by first selecting the row that will be
edited or removed, and then selecting the desired action. Editing will
bring up a dialogue box to edit the row data, and the delete will bring
up a dialogue box asking if you wish to proceed.

## Table Information {.tabset}

### Case Files

Case Files: The main table that contains information on each case.
Column information is as follows (if self explanatory no additional
information is provided):

-   Case File Number - The main key from which all tables are connected
    too.
-   Case File Name
-   Episode Number
-   Date Statement Recorded - The date the original statement was taken
    by the Magnus Institute.
-   Date Event Started - Date the event started.
-   Date Event Finished - Date the event ended.
-   Exact Dates Known - True or False, used to determine how accurate
    the dates given are.
-   Statement Provider Name
-   Statement Provider Alive - True or False, assume true unless stated
    otherwise.
-   Magnus Recorded Date - The date on which the statement was
    re-recorded, which is the same as the date the podcast was released.
-   Magnus Event - A sequential number starting with one, that tracks
    significant events occurring at or to people at the Magnus Archives.
    Used to identify information in the Magnus Events table.
-   URL - The URL that links to the podcast at Rusty Quill. Used on the
    map section.
-   Comments - These are any comments that user wants to make about the
    story.

### Event Locations

Event Locations: Table that contains the locations at which events took
place. The table uses the Case File Number to track which location
belongs to which event. Multiple locations can be provided to a single
location.

-   Case File Number - Used to link back to Case File table.
-   Latitude - The latitude in WGS 84 coordinates.
-   Longitude - The longitude in WGS 84 coordinates.
-   Location Name

### Persons of Interest

Persons of Interest: Table that contains all people or entities that are
of interest to the case file. This table can be used to cross-reference
the case files looking for specific people or groups.

-   Case File Number - Used to link back to Case File table.
-   Name - Name of person, group, entity, etc.

### Magnus Events

Magnus Event: Table that records events that occur in "real time" at or
to people at the Magnus Archives. The idea for this table is that both
the individual cases and the overall narrative will be tracked and
recorded. The date of the event is assumed to be the same as the date
the event is being re-recorded (Magnus Recorded Date).

-   Magnus Event - The number assigned to each event. Used to connect to
    Case File table.
-   Event Name - Name the user wishes to provide to the event.
-   Notes about Event - Any notes or information the user wishes to keep
    about the event.

### Connected Cases

Connected Cases: Table that contains any cases that are connected to
other cases. Connection can consist of three different levels: Strong,
Weak, or Possible. This table is used to graph the connections between
cases to begin to group and cluster the results based on events
occurring.

-   Case File Number - Used to link back to Case File table.
-   Connected Case File Number - Case File Number that is connected to
    the chosen case.
-   Connection Type - How strong is the connection. Strong - Indicates
    no doubt. Weak - Certain people or similarities are present but not
    a definitive connection. Possible - Used as a place holder if user
    is not sure of a connection, but believes there could be one.
