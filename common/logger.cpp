#include "logger.hpp"

LogStream::LogStream(std::ostream* output, std::mutex& mutex, bool enabled)
    : output(output)
    , mutex(mutex)
    , enabled(enabled)
{
    mutex.lock();
}

LogStream::~LogStream()
{
    if (!moved && enabled && output)
    {
        *output << buffer.str() << std::endl;
    }
    if (!moved)
    {
        mutex.unlock();
    }
}

LogStream::LogStream(LogStream&& other) noexcept
    : output(other.output)
    , mutex(other.mutex)
    , buffer(std::move(other.buffer))
    , enabled(other.enabled)
    , moved(false)
{
    other.moved = true;
}


std::array<Logger::ChannelState, static_cast<size_t>(LogChannel::_Count)> Logger::channels = {};
std::mutex Logger::mutex;

LogStream Logger::log(LogChannel channel)
{
    auto index = static_cast<size_t>(channel);
    auto& state = channels[index];
    return LogStream(state.output, mutex, state.enabled);
}

void Logger::enable(LogChannel channel)
{
    std::lock_guard<std::mutex> lock(mutex);
    channels[static_cast<size_t>(channel)].enabled = true;
}

void Logger::disable(LogChannel channel)
{
    std::lock_guard<std::mutex> lock(mutex);
    channels[static_cast<size_t>(channel)].enabled = false;
}

void Logger::set_output(LogChannel channel, std::ostream* output)
{
    std::lock_guard<std::mutex> lock(mutex);
    channels[static_cast<size_t>(channel)].output = output;
}