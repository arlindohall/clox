cmake_minimum_required(VERSION 3.10)

project(clox)

file(GLOB MyCSources clox/*.c)
add_executable(clox ${MyCSources})
