####################################################
# DEPENDENCIES

add_subdirectory(third-party)

####################################################

add_subdirectory(src)

####################################################
# The main executable

add_executable(MAIN main.cpp)

target_link_libraries(
    MAIN 
    PUBLIC 
        DEPENDENCIES 
        UALG
        DIRACOQ
)

# Add the "run" target to execute the built executable
add_custom_target(run
    COMMAND ./MAIN
    DEPENDS MAIN
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running the MAIN executable"
)

####################################################

add_executable(TOP toplevel.cpp)

target_link_libraries(
    TOP
    PUBLIC
        DEPENDENCIES
        UALG
        DIRACOQ
)


add_custom_target(toplevel
    COMMAND ./TOP "output.txt"
    DEPENDS TOP
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running the TOPLEVEL executable"
)