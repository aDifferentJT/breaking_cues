import 'dart:async';
import 'dart:math';

import 'package:built_collection/built_collection.dart';
import 'package:core/pubsub.dart';
import 'package:flutter/material.dart';
import 'package:intersperse/intersperse.dart';

import 'package:core/deck.dart';
import 'package:core/message.dart';
import 'package:flutter_utils/widget_modifiers.dart';
import 'package:output/arch.dart';

import 'countdown.dart';
import 'caption_state.dart';
import 'caption_transition.dart';
import 'colour_to_flutter.dart';
import 'display_settings_flutter.dart';

@immutable
class _CountdownText extends StatelessWidget {
  final CountdownState countdown;
  final TextStyle textStyle;
  final TextAlign textAlign;
  final Color backgroundColour;

  const _CountdownText(
    this.countdown, {
    required this.textStyle,
    required this.textAlign,
    required this.backgroundColour,
  });

  @override
  Widget build(BuildContext context) {
    return countdown.isStopped
        ? Text(
            countdown.slide.whenStopped,
            style: textStyle,
            textAlign: textAlign,
          )
        : Text.rich(
            TextSpan(
              children: (countdown.slide.message.split('#'))
                  .map((text) => TextSpan(text: text))
                  .cast<InlineSpan>()
                  .intersperse(
                    WidgetSpan(
                      alignment: PlaceholderAlignment.baseline,
                      baseline: TextBaseline.alphabetic,
                      child: DigitalCountdown(
                        remaining: countdown.remaining,
                        style: textStyle,
                      ),
                    ),
                  )
                  .toList(growable: false),
            ),
            style: textStyle,
            textAlign: textAlign,
          );
  }
}

@immutable
class _CountdownView extends StatelessWidget {
  final CountdownState countdown;
  final Color backgroundColour;

  const _CountdownView(
    this.countdown, {
    required this.backgroundColour,
  });

  @override
  Widget build(BuildContext context) {
    switch (countdown.displaySettings.style) {
      case Style.none:
        return const SizedBox.shrink();

      case Style.topLines:
        return Stack(children: [
          Arch(colour: countdown.displaySettings.backgroundColour.flutter)
              .positionedFill,
          Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Text(
                countdown.slide.subtitle1,
                style: countdown.displaySettings.bodyStyle,
                textAlign: TextAlign.center,
              ),
              Divider(
                height: countdown.displaySettings.subtitleSize / 2,
                color: countdown.displaySettings.textColour.flutter,
              ),
              Text(
                countdown.slide.subtitle2,
                style: countdown.displaySettings.bodyStyle,
                textAlign: TextAlign.center,
              ),
              const Spacer(),
            ],
          )
              .constrained(const BoxConstraints(maxWidth: 480))
              .intrinsicWidth()
              .padding(const EdgeInsets.all(32))
              .fractionallySized(heightFactor: 2 / 3),
          _CountdownText(
            countdown,
            textStyle: countdown.displaySettings.bodyStyle,
            textAlign: TextAlign.right,
            backgroundColour: backgroundColour,
          )
              .constrained(const BoxConstraints(maxWidth: 480))
              .container(
                alignment: Alignment.topRight,
                padding: const EdgeInsets.all(32),
              )
              .fractionallySized(heightFactor: 2 / 3),
          Text(
            countdown.slide.title,
            style: countdown.displaySettings.titleStyle,
            textAlign: TextAlign.center,
          )
              .centered()
              .constrained(const BoxConstraints(maxWidth: 960))
              .fractionallySized(
                alignment: Alignment.topCenter,
                heightFactor: 1 / 3,
              )
              .positionedFill,
        ]).fractionallySized(
          alignment: Alignment.topCenter,
          heightFactor: 1 / 2,
        );

      case Style.leftQuarter:
      case Style.leftThird:
      case Style.leftHalf:
      case Style.leftTwoThirds:
      case Style.rightQuarter:
      case Style.rightThird:
      case Style.rightHalf:
      case Style.rightTwoThirds:
      case Style.bottomLines:
      case Style.bottomParagraphs:
      case Style.fullScreen:
        return Stack(children: [
          Column(children: [
            const Spacer(flex: 1),
            Text(
              countdown.slide.title,
              style: countdown.displaySettings.titleStyle,
              textAlign: TextAlign.center,
            ).centered(),
            const Spacer(flex: 1),
            AnalogueCountdown(
              remaining: countdown.remaining,
              strokeWidth: 20,
              colour: countdown.displaySettings.textColour.flutter,
            ).expanded(flex: 6),
            const Spacer(flex: 1),
            _CountdownText(
              countdown,
              textStyle: countdown.displaySettings.titleStyle,
              textAlign: TextAlign.center,
              backgroundColour: backgroundColour,
            ),
            const Spacer(flex: 1),
          ]).container(color: backgroundColour).positionedFill,
        ]).opacity(countdown.opacity);
    }
  }
}

@immutable
class Output extends StatefulWidget {
  final PubSub<Message> pubSub;
  final String name;

  const Output({
    super.key,
    required this.pubSub,
    required this.name,
  });

  @override
  createState() => OutputState();
}

class OutputState extends State<Output> with SingleTickerProviderStateMixin {
  CaptionTransition _captionTransition =
      CaptionTransition.constant(CaptionState.closed());

  Timer? _updateTimer;

  late final AnimationController _captionDecorationController;
  late StreamSubscription<Message> _streamSubscription;

  var _defaultSettings = BuiltMap<String, DisplaySettings>();
  DeckIndex? _deckIndex;

  void show({required bool quiet, required DeckIndex deckIndex}) {
    _captionDecorationController.stop();

    _captionTransition = CaptionTransition.show(
      previousState: _captionTransition.evaluate(_captionDecorationController),
      quiet: quiet,
      deckIndex: deckIndex,
      defaultSettings: _defaultSettings,
      name: widget.name,
    );
    _captionDecorationController.reset();
    _captionDecorationController.duration = _captionTransition.duration;
    _captionDecorationController.forward();

    _updateTimer?.cancel();
    _updateTimer = deckIndex.slide is CountdownChunk
        ? Timer.periodic(
            const Duration(milliseconds: 500),
            (_) => setState(() {}),
          )
        : null;
  }

  void close() {
    _captionDecorationController.stop();
    _captionTransition = CaptionTransition.close(
        _captionTransition.evaluate(_captionDecorationController));
    _captionDecorationController.reset();
    _captionDecorationController.duration = _captionTransition.duration;
    _captionDecorationController.forward();
  }

  void process(message) {
    if (message is ShowMessage) {
      _defaultSettings = message.defaultSettings;
      _deckIndex = message.deckIndex;
      show(quiet: message.quiet, deckIndex: message.deckIndex);
    } else if (message is CloseMessage) {
      _deckIndex = null;
      close();
    } else {
      throw ArgumentError.value(message, 'Message type not recognised');
    }
  }

  @override
  void initState() {
    super.initState();

    _captionDecorationController =
        AnimationController(duration: _captionTransition.duration, vsync: this)
          ..addListener(() => setState(() {}));

    _streamSubscription = widget.pubSub.subscribe(process);
  }

  @override
  void didUpdateWidget(covariant Output oldWidget) {
    super.didUpdateWidget(oldWidget);

    if (widget.pubSub != oldWidget.pubSub) {
      _streamSubscription.cancel();
      _streamSubscription = widget.pubSub.subscribe(process);
    }

    if (oldWidget.name != widget.name) {
      if (_deckIndex == null) {
        close();
      } else {
        show(quiet: true, deckIndex: _deckIndex!);
      }
    }
  }

  @override
  dispose() {
    _captionDecorationController.dispose();
    _streamSubscription.cancel();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    final countdown =
        _captionTransition.evaluate(_captionDecorationController).countdown;

    return SizedBox(
      width: 1920,
      height: 1080,
      child: Stack(children: [
        Stack(
          children: _captionTransition
              .evaluate(_captionDecorationController)
              .overlayLayers
              .entries
              .map((entry) {
            return entry.key.aligned(entry.value).padding(
                  _captionTransition
                      .evaluate(_captionDecorationController)
                      .edgeInsets,
                );
          }).toList(growable: false),
        )
            .decorated(
              decoration: BoxDecoration(
                color: _captionTransition.backgroundColour
                    .evaluate(_captionDecorationController),
                borderRadius: BorderRadius.only(
                  topLeft: Radius.circular(_captionTransition.rrect
                      .evaluate(_captionDecorationController)
                      .topLeftRadius),
                  topRight: Radius.circular(_captionTransition.rrect
                      .evaluate(_captionDecorationController)
                      .topRightRadius),
                  bottomLeft: Radius.circular(_captionTransition.rrect
                      .evaluate(_captionDecorationController)
                      .bottomLeftRadius),
                  bottomRight: Radius.circular(_captionTransition.rrect
                      .evaluate(_captionDecorationController)
                      .bottomRightRadius),
                ),
              ),
            )
            .positioned(
              left: _captionTransition.rrect
                  .evaluate(_captionDecorationController)
                  .left,
              right: _captionTransition.rrect
                  .evaluate(_captionDecorationController)
                  .right,
              top: _captionTransition.rrect
                  .evaluate(_captionDecorationController)
                  .top,
              bottom: _captionTransition.rrect
                  .evaluate(_captionDecorationController)
                  .bottom,
            ),
        if (countdown != null)
          _CountdownView(
            countdown,
            backgroundColour: _captionTransition.backgroundColour
                .evaluate(_captionDecorationController)!,
          ).positionedFill,
      ]),
    );
  }
}

class ScaledOutput extends StatelessWidget {
  final PubSub<Message> pubSub;
  final String name;

  const ScaledOutput({
    super.key,
    required this.pubSub,
    required this.name,
  });

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(builder: (context, constraints) {
      return Transform.scale(
        scale: min(
          constraints.maxWidth / 1920,
          constraints.maxHeight / 1080,
        ),
        child: OverflowBox(
          maxWidth: 1920,
          maxHeight: 1080,
          child: Output(
            pubSub: pubSub,
            name: name,
          ),
        ),
      );
    });
  }
}
