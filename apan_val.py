#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed May  1 12:13:17 2024

@author: ttran
"""

import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# PUL white
cm_white = np.array([[119, 171],
                     [11, 1872]])

sns.set(font_scale=1.2)
ax = sns.heatmap(cm_white, annot=True, fmt='d', cmap='Blues', xticklabels=[], yticklabels=[])

# Add labels on the sides
ax.text(-0.25, 1.5, "0", ha='center', va='center', fontsize=14)
ax.text(0.5, 0.7, "(5.5%)", ha='center', va='center', fontsize=14) # upper left
ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=14)
ax.text(0.5, 1.7, "(1%)", ha='center', va='center', fontsize=14) # lower left

#ax.text(-0.25, 1.5, "25%", ha='center', va='center', fontsize=12)
#ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=12)

# Add labels for the sides
plt.text(-0.5, 1, 'Predicted', ha='right', va='center', rotation=90, fontsize=14)
plt.text(1, -0.5, 'Actual', ha='center', va='top', fontsize=14)

# Add labels for the top side
ax.text(0.5, -0.25, "1", ha='center', va='center', fontsize=14)
ax.text(1.5, 0.7, "(7.9%)", ha='center', va='center', fontsize=14) # upper right
ax.text(1.5, -0.25, "0", ha='center', va='center', fontsize=14)
ax.text(1.5, 1.7, "(86.1%)", ha='center', va='center', fontsize=14, color="white") # lower right
# Display the plot
plt.show()

# black
cm_black = np.array([[70, 111],
                     [5, 560]])

sns.set(font_scale=1.2)
ax = sns.heatmap(cm_black, annot=True, fmt='d', cmap='Blues', xticklabels=[], yticklabels=[])

# Add labels on the sides
ax.text(-0.25, 1.5, "0", ha='center', va='center', fontsize=14)
ax.text(0.5, 0.7, "(9.4%)", ha='center', va='center', fontsize=14) # upper left
ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=14)
ax.text(0.5, 1.7, "(1%)", ha='center', va='center', fontsize=14) # lower left

#ax.text(-0.25, 1.5, "25%", ha='center', va='center', fontsize=12)
#ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=12)

# Add labels for the sides
plt.text(-0.5, 1, 'Predicted', ha='right', va='center', rotation=90, fontsize=14)
plt.text(1, -0.5, 'Actual', ha='center', va='top', fontsize=14)

# Add labels for the top side
ax.text(0.5, -0.25, "1", ha='center', va='center', fontsize=14)
ax.text(1.5, 0.7, "(14.9%)", ha='center', va='center', fontsize=14) # upper right
ax.text(1.5, -0.25, "0", ha='center', va='center', fontsize=14)
ax.text(1.5, 1.7, "(75.1%)", ha='center', va='center', fontsize=14, color="white") # lower right
# Display the plot
plt.show()

# HL
cm_hispanic = np.array([[92, 105],
                     [5, 896]])

sns.set(font_scale=1.2)
ax = sns.heatmap(cm_hispanic, annot=True, fmt='d', cmap='Blues', xticklabels=[], yticklabels=[])

# Add labels on the sides
ax.text(-0.25, 1.5, "0", ha='center', va='center', fontsize=14)
ax.text(0.5, 0.7, "(8.4%)", ha='center', va='center', fontsize=14) # upper left
ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=14)
ax.text(0.5, 1.7, "(0.5%)", ha='center', va='center', fontsize=14) # lower left

#ax.text(-0.25, 1.5, "25%", ha='center', va='center', fontsize=12)
#ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=12)

# Add labels for the sides
plt.text(-0.5, 1, 'Predicted', ha='right', va='center', rotation=90, fontsize=14)
plt.text(1, -0.5, 'Actual', ha='center', va='top', fontsize=14)

# Add labels for the top side
ax.text(0.5, -0.25, "1", ha='center', va='center', fontsize=14)
ax.text(1.5, 0.7, "(9.6%)", ha='center', va='center', fontsize=14) # upper right
ax.text(1.5, -0.25, "0", ha='center', va='center', fontsize=14)
ax.text(1.5, 1.7, "(81.6%)", ha='center', va='center', fontsize=14, color="white") # lower right
# Display the plot
plt.show()

#### CTGAN step 3
cm_white = np.array([[150, 140],
                     [4, 1531]])

sns.set(font_scale=1.2)
ax = sns.heatmap(cm_white, annot=True, fmt='d', cmap='Blues', xticklabels=[], yticklabels=[])

# Add labels on the sides
ax.text(-0.25, 1.5, "0", ha='center', va='center', fontsize=14)
ax.text(0.5, 0.7, "(8.2%)", ha='center', va='center', fontsize=14) # upper left
ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=14)
ax.text(0.5, 1.7, "(0.2%)", ha='center', va='center', fontsize=14) # lower left

#ax.text(-0.25, 1.5, "25%", ha='center', va='center', fontsize=12)
#ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=12)

# Add labels for the sides
plt.text(-0.5, 1, 'Predicted', ha='right', va='center', rotation=90, fontsize=14)
plt.text(1, -0.5, 'Actual', ha='center', va='top', fontsize=14)

# Add labels for the top side
ax.text(0.5, -0.25, "1", ha='center', va='center', fontsize=14)
ax.text(1.5, 0.7, "(7.7%)", ha='center', va='center', fontsize=14) # upper right
ax.text(1.5, -0.25, "0", ha='center', va='center', fontsize=14)
ax.text(1.5, 1.7, "(83.9%)", ha='center', va='center', fontsize=14, color="white") # lower right
# Display the plot
plt.show()

# black
cm_black = np.array([[86, 95],
                     [3, 554]])

sns.set(font_scale=1.2)
ax = sns.heatmap(cm_black, annot=True, fmt='d', cmap='Blues', xticklabels=[], yticklabels=[])

# Add labels on the sides
ax.text(-0.25, 1.5, "0", ha='center', va='center', fontsize=14)
ax.text(0.5, 0.7, "(11.7%)", ha='center', va='center', fontsize=14) # upper left
ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=14)
ax.text(0.5, 1.7, "(0.4%)", ha='center', va='center', fontsize=14) # lower left

#ax.text(-0.25, 1.5, "25%", ha='center', va='center', fontsize=12)
#ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=12)

# Add labels for the sides
plt.text(-0.5, 1, 'Predicted', ha='right', va='center', rotation=90, fontsize=14)
plt.text(1, -0.5, 'Actual', ha='center', va='top', fontsize=14)

# Add labels for the top side
ax.text(0.5, -0.25, "1", ha='center', va='center', fontsize=14)
ax.text(1.5, 0.7, "(12.9%)", ha='center', va='center', fontsize=14) # upper right
ax.text(1.5, -0.25, "0", ha='center', va='center', fontsize=14)
ax.text(1.5, 1.7, "(75.1%)", ha='center', va='center', fontsize=14, color="white") # lower right
# Display the plot
plt.show()

# HL
cm_hispanic = np.array([[111, 86],
                     [9, 968]])

sns.set(font_scale=1.2)
ax = sns.heatmap(cm_hispanic, annot=True, fmt='d', cmap='Blues', xticklabels=[], yticklabels=[])

# Add labels on the sides
ax.text(-0.25, 1.5, "0", ha='center', va='center', fontsize=14)
ax.text(0.5, 0.7, "(9.4%)", ha='center', va='center', fontsize=14) # upper left
ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=14)
ax.text(0.5, 1.7, "(1%)", ha='center', va='center', fontsize=14) # lower left

#ax.text(-0.25, 1.5, "25%", ha='center', va='center', fontsize=12)
#ax.text(-0.25, 0.5, "1", ha='center', va='center', fontsize=12)

# Add labels for the sides
plt.text(-0.5, 1, 'Predicted', ha='right', va='center', rotation=90, fontsize=14)
plt.text(1, -0.5, 'Actual', ha='center', va='top', fontsize=14)

# Add labels for the top side
ax.text(0.5, -0.25, "1", ha='center', va='center', fontsize=14)
ax.text(1.5, 0.7, "(7.3%)", ha='center', va='center', fontsize=14) # upper right
ax.text(1.5, -0.25, "0", ha='center', va='center', fontsize=14)
ax.text(1.5, 1.7, "(82.2%)", ha='center', va='center', fontsize=14, color="white") # lower right
# Display the plot
plt.show()

