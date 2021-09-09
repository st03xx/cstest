cmake_minimum_required(VERSION 3.10)

project(cstest)

add_executable(
    cstest
    main.cpp
)

if (UNIX)
target_link_libraries(
    cstest
    dl
    pthread
)
endif (UNIX)
