
# ANTLR
file(GLOB_RECURSE 
    ANTLR_SRCS 
    "${CMAKE_CURRENT_SOURCE_DIR}/antlr_runtime/*.cpp")

add_library(
    DEPENDENCIES
    ${ANTLR_SRCS}
)

target_include_directories(
    DEPENDENCIES 
    PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/antlr_runtime
    ${CMAKE_CURRENT_SOURCE_DIR}/Wolfram)


# MacOS
if (APPLE)
    target_link_libraries(
        DEPENDENCIES
        PUBLIC
            ${CMAKE_CURRENT_SOURCE_DIR}/Wolfram/libWSTPi4.a
            "-framework CoreFoundation" # for apple systems
    )

# Linux
elseif (UNIX)
    target_link_libraries(
        DEPENDENCIES
        PUBLIC
            ${CMAKE_CURRENT_SOURCE_DIR}/Wolfram/libWSTP64i4.so
            "-lstdc++fs" # for linux systems
    )

else()
    message(FATAL_ERROR "Unsupported platform. Please use MacOS or Linux.")
endif()