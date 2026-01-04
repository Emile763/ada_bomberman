#ifndef CHECK_RESIZE_H
#define CHECK_RESIZE_H

// Functions used to know if the terminal has been resized
// Only works on Unix for now

/// @brief Init the resize handler
void initResizeHandler ();

/// @brief Verify if the terminal has been resized since the last check
/// @return 1 if resized else 0
int checkResized ();

#endif