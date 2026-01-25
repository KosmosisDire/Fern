#pragma once

#include <iostream>
#include <sstream>
#include <mutex>
#include <array>

enum class LogChannel
{
    General,
    Debug,
    _Count
};

class LogStream
{
public:
    LogStream(std::ostream* output, std::mutex& mutex, bool enabled);
    ~LogStream();

    LogStream(const LogStream&) = delete;
    LogStream& operator=(const LogStream&) = delete;
    LogStream(LogStream&& other) noexcept;
    LogStream& operator=(LogStream&&) = delete;

    template<typename T>
    LogStream& operator<<(const T& value)
    {
        if (enabled)
        {
            buffer << value;
        }
        return *this;
    }

private:
    std::ostream* output;
    std::mutex& mutex;
    std::ostringstream buffer;
    bool enabled;
    bool moved = false;
};

class Logger
{
public:
    static LogStream log(LogChannel channel);
    static void enable(LogChannel channel);
    static void disable(LogChannel channel);
    static void set_output(LogChannel channel, std::ostream* output);

private:
    struct ChannelState
    {
        bool enabled = true;
        std::ostream* output = &std::cout;
    };

    static std::array<ChannelState, static_cast<size_t>(LogChannel::_Count)> channels;
    static std::mutex mutex;
};


#define LOG(channel) Logger::log(channel)